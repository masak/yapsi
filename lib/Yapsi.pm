use v6;

grammar Yapsi::Perl6::Grammar {
    regex TOP { ^ <statement> ** ';' $ }
    token statement { <expression> || '' }
    token expression { <assignment> || <binding> || <variable> || <literal>
                       || <declaration> || <saycall> }
    token lvalue { <declaration> || <variable> }
    token variable { '$' \w+ }
    token literal { \d+ }
    rule  declaration { 'my' <variable> }
    rule  assignment { <lvalue> '=' <expression> }
    rule  binding { <lvalue> ':=' <expression> }
    rule  saycall { 'say' <expression> }  # very temporary solution
}

my %d; # a variable gets an entry in %d when it's declared

multi sub find-vars(Match $/, 'statement') {
    if $<expression> -> $e {
        find-vars($e, 'expression');
    }
}

multi sub find-vars(Match $/, 'expression') {
    for <assignment binding variable declaration saycall> -> $subrule {
        if $/{$subrule} -> $e {
            find-vars($e, $subrule);
        }
    }
}

multi sub find-vars(Match $/, 'lvalue') {
    for <variable declaration> -> $subrule {
        if $/{$subrule} -> $e {
            find-vars($e, $subrule);
        }
    }
}

multi sub find-vars(Match $/, 'variable') {
    if !%d.exists( ~$/ ) {
        die "Invalid. $/ not declared before use";
    }
}

multi sub find-vars(Match $/, 'literal') {
    die "This multi variant should never be called";
}

multi sub find-vars(Match $/, 'declaration') {
    my $name = ~$<variable>;
    if %d{$name}++ {
        $*ERR.say: "Useless redeclaration of variable $name";
    }
}

multi sub find-vars(Match $/, 'assignment') {
    find-vars($<lvalue>, 'lvalue');
    find-vars($<expression>, 'expression');
}

multi sub find-vars(Match $/, 'binding') {
    find-vars($<lvalue>, 'lvalue');
    find-vars($<expression>, 'expression');
}

multi sub find-vars(Match $/, 'saycall') {
    find-vars($<expression>, 'expression');
}

multi sub find-vars($/, $node) {
    die "Don't know what to do with a $node";
}

my $c;
sub unique-register {
    return '$' ~ $c++;
}

my @sic;

multi sub sicify(Match $/, 'statement') {
    if $<expression> -> $e {
        return sicify($e, 'expression');
    }
}

multi sub sicify(Match $/, 'expression') {
    for <variable literal declaration assignment binding saycall> -> $subrule {
        if $/{$subrule} -> $e {
            return sicify($e, $subrule);
        }
    }
}

multi sub sicify(Match $/, 'lvalue') {
    for <variable declaration> -> $subrule {
        if $/{$subrule} -> $e {
            return sicify($e, $subrule);
        }
    }
}

multi sub sicify(Match $/, 'variable') {
    my $register = unique-register;
    my $variable = "'$/'";
    push @sic, "$register = fetch $variable";
    return ($register, $variable);
}

multi sub sicify(Match $/, 'literal') {
    my $register = unique-register;
    my $literal = ~$/;
    push @sic, "$register = $literal";
    return ($register, $literal);
}

multi sub sicify(Match $/, 'declaration') {
    return sicify($<variable>, 'variable');
}

multi sub sicify(Match $/, 'assignment') {
    my ($register, $) = sicify($<expression>, 'expression');
    my ($, $variable) = sicify($<lvalue>, 'lvalue');
    push @sic, "store $variable, $register";
    return ($register, $variable);
}

multi sub sicify(Match $/, 'binding') {
    my ($register, $rightvar) = sicify($<expression>, 'expression');
    my ($, $leftvar) = sicify($<lvalue>, 'lvalue');
    if $rightvar ~~ / ^ \d+ $ / { # hm. this is brittle and suboptimal.
        $rightvar = $register;
    }
    push @sic, "bind $leftvar, $rightvar";
    return ($register, $leftvar);
}

multi sub sicify(Match $/, 'saycall') {
    my ($register, $) = sicify($<expression>, 'expression');
    my $result = unique-register;
    push @sic, "say $register";
    push @sic, "$result = 1";
    return ($result, 1);
}

multi sub sicify(Match $/, $node) {
    die "Don't know what to do with a $node";
}

sub declutter(@instructions) {
    my @decluttered;
    for @instructions.kv -> $i, $line {
        if $line !~~ / ^ ('$' \d+) ' =' / {
            push @decluttered, $line;
        }
        else {
            my $varname = ~$0;
            my Bool $usages-later = False;
            for $i+1 ..^ @instructions -> $j {
                ++$usages-later if defined index(@instructions[$j], $varname);
            }
            if $usages-later {
                push @decluttered, $line;
            }
        }
    }
    return @decluttered;
}

sub renumber(@instructions) {
    my $number = 0;
    my %mapping;
    return @instructions.map: {
        .subst( :global, / ('$' \d+) /, {
            my $varname = ~$0;
            if !%mapping.exists($varname) {
                %mapping{$varname} = '$' ~ $number++;
            }
            %mapping{$varname}
        } );
    };
}

class Yapsi::Compiler {
    method compile($program) {
        die "Could not parse"
            unless Yapsi::Perl6::Grammar.parse($program);
        %d = ();
        for $<statement> -> $statement {
            find-vars($statement, 'statement');
        }
        $c = 0;
        @sic = '`lexicals: <' ~ (join ' ', %d.keys) ~ '>';
        for $<statement> -> $statement {
            sicify($statement, 'statement');
        }
        @sic = renumber(declutter(@sic));
        return @sic;
    }
}

class Yapsi::Runtime::IO {
    method say($message) {
        # RAKUDO: Can't use say in a method say [perl #74014]
        print $message, "\n";
    }
}

class Yapsi::Runtime {
    has Yapsi::Runtime::IO $!IO;

    method run(@sic) {
        my @r;
        my %pad;
        my @containers;
        for @sic {
            when /^ '`lexicals: <' ('' || \S+ ** \s) '>' $ / {
                for $0.comb(/\S+/) -> $var {
                    %pad{$var} = { :type<container>, :n(+@containers) };
                    push @containers, 'Any()';
                }
            }
            when /^ '$'(\d+) ' = ' (\d+) $/ {
                @r[+$0] = $1
            }
            when /^ 'store ' \'(<-[']>+)\' ', $'(\d+) $/ {
                my $thing = %pad{~$0};
                if $thing<type> eq 'container' {
                    my $n = $thing<n>;
                    @containers[$n] = @r[+$1];
                }
                else {
                    die "Cannot store something in readonly symbol ~$0";
                }
            }
            when /^ '$'(\d+) ' = fetch '\'(<-[']>+)\' $/ {
                my $thing = %pad{~$1};
                if $thing<type> eq 'container' {
                    my $n = $thing<n>;
                    @r[+$0] = @containers[$n];
                }
                else { # immadiate
                    @r[+$0] = $thing<value>;
                }
            }
            when /^ 'bind ' \'(<-[']>+)\' ', ' \'(<-[']>+)\' $/ {
                %pad{~$0} = %pad{~$1};
            }
            when /^ 'bind ' \'(<-[']>+)\' ', $'(\d+) $/ {
                %pad{~$0} = { :type<immediate>, :value(+$1) };
            }
            when /^ 'say $'(\d+) $/ {
                $!IO.say: @r[+$0];
            }
            default { die "Couldn't handle instruction `$_`" }
        }
    }
}
