use v6;

grammar Yapsi::Perl6::Grammar {
    regex TOP { ^ <statement> ** ';' <.ws> $ }
    token statement { <expression> || '' }
    token expression { <assignment> || <binding> || <variable> || <literal>
                       || <declaration> || <saycall> || <increment> }
    token lvalue { <declaration> || <variable> || <increment> }
    token value { <variable> || <literal> || <declaration> || <saycall>
                  || <increment> }
    token variable { '$' \w+ }
    token literal { \d+ }
    rule  declaration { 'my' <variable> }
    rule  assignment { <lvalue> '=' <expression> }
    rule  binding { <lvalue> ':=' <expression> }
    rule  saycall { 'say' <expression> }  # very temporary solution
    rule  increment { '++' <value> }
}

class Yapsi::Compiler {
    has @.warnings;

    has %!d;   # a variable gets an entry in %!d when it's declared
    has $!c;   # unique register counter
    has @!sic; # generated SIC statements

    method compile($program) {
        @!warnings = ();
        die "Could not parse"
            unless Yapsi::Perl6::Grammar.parse($program);
        %!d = ();
        for $<statement> -> $statement {
            self.find-vars($statement, 'statement');
        }
        $!c = 0;
        @!sic = '`lexicals: <' ~ (join ' ', %!d.keys) ~ '>';
        for $<statement> -> $statement {
            self.sicify($statement, 'statement');
        }
        return renumber(declutter(@!sic));
    }

    multi method find-vars(Match $/, 'statement') {
        if $<expression> -> $e {
            self.find-vars($e, 'expression');
        }
    }

    multi method find-vars(Match $/, 'expression') {
        for <assignment binding variable declaration saycall
             increment> -> $subrule {
            if $/{$subrule} -> $e {
                self.find-vars($e, $subrule);
            }
        }
    }

    multi method find-vars(Match $/, 'lvalue') {
        for <variable declaration> -> $subrule {
            if $/{$subrule} -> $e {
                self.find-vars($e, $subrule);
            }
        }
    }

    multi method find-vars(Match $/, 'value') {
        for <variable declaration saycall increment> -> $subrule {
            if $/{$subrule} -> $e {
                self.find-vars($e, $subrule);
            }
        }
    }

    multi method find-vars(Match $/, 'variable') {
        unless %!d.exists( ~$/ ) {
            die "Invalid. $/ not declared before use";
        }
    }

    multi method find-vars(Match $/, 'literal') {
        die "This multi variant should never be called";
    }

    multi method find-vars(Match $/, 'declaration') {
        my $name = ~$<variable>;
        if %!d{$name}++ {
            @!warnings.push: "Useless redeclaration of variable $name";
        }
    }

    multi method find-vars(Match $/, 'assignment') {
        self.find-vars($<lvalue>, 'lvalue');
        self.find-vars($<expression>, 'expression');
    }

    multi method find-vars(Match $/, 'binding') {
        self.find-vars($<lvalue>, 'lvalue');
        self.find-vars($<expression>, 'expression');
    }

    multi method find-vars(Match $/, 'saycall') {
        self.find-vars($<expression>, 'expression');
    }

    multi method find-vars(Match $/, 'increment') {
        self.find-vars($<value>, 'value');
    }

    multi method find-vars($/, $node) {
        die "Don't know what to do with a $node";
    }

    method unique-register {
        return '$' ~ $!c++;
    }

    multi method sicify(Match $/, 'statement') {
        if $<expression> -> $e {
            return self.sicify($e, 'expression');
        }
    }

    multi method sicify(Match $/, 'expression') {
        for <variable literal declaration assignment binding saycall
             increment> -> $subrule {
            if $/{$subrule} -> $e {
                return self.sicify($e, $subrule);
            }
        }
    }

    multi method sicify(Match $/, 'lvalue') {
        for <variable declaration increment> -> $subrule {
            if $/{$subrule} -> $e {
                return self.sicify($e, $subrule);
            }
        }
    }

    multi method sicify(Match $/, 'value') {
        for <variable literal declaration saycall increment> -> $subrule {
            if $/{$subrule} -> $e {
                return self.sicify($e, $subrule);
            }
        }
    }

    multi method sicify(Match $/, 'variable') {
        my $register = self.unique-register;
        my $variable = "'$/'";
        push @!sic, "$register = fetch $variable";
        return ($register, $variable);
    }

    multi method sicify(Match $/, 'literal') {
        my $register = self.unique-register;
        my $literal = ~$/;
        push @!sic, "$register = $literal";
        return ($register, '<constant>');
    }

    multi method sicify(Match $/, 'declaration') {
        return self.sicify($<variable>, 'variable');
    }

    multi method sicify(Match $/, 'assignment') {
        my ($register, $) = self.sicify($<expression>, 'expression');
        my ($, $variable) = self.sicify($<lvalue>, 'lvalue');
        push @!sic, "store $variable, $register";
        return ($register, $variable);
    }

    multi method sicify(Match $/, 'binding') {
        my ($register, $rightvar) = self.sicify($<expression>, 'expression');
        my ($, $leftvar) = self.sicify($<lvalue>, 'lvalue');
        if $rightvar ~~ / ^ \d+ $ / { # hm. this is brittle and suboptimal.
            $rightvar = $register;
        }
        push @!sic, "bind $leftvar, $rightvar";
        return ($register, $leftvar);
    }

    multi method sicify(Match $/, 'saycall') {
        my ($register, $) = self.sicify($<expression>, 'expression');
        my $result = self.unique-register;
        push @!sic, "say $register";
        push @!sic, "$result = 1";
        return ($result, 1);
    }

    multi method sicify(Match $/, 'increment') {
        my ($register, $variable) = self.sicify($<value>, 'value');
        die "Can't increment a constant"
            if $variable eq '<constant>';
        push @!sic, "inc $register";
        push @!sic, "store $variable, $register";
        return ($register, $variable);
    }

    multi method sicify(Match $/, $node) {
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
                    ++$usages-later
                        if defined index(@instructions[$j], $varname);
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
}

subset Yapsi::IO where { .can('say') }

class Yapsi::Runtime {
    has Yapsi::IO $!io = $*OUT;

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
                @r[+$0] = +$1
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
                $!io.say: @r[+$0];
            }
            when /^ 'inc $'(\d+) $/ {
                if @r[+$0] eq 'Any()' {
                    @r[+$0] = 1;
                }
                else {
                    ++@r[+$0];
                }
            }
            default { die "Couldn't handle instruction `$_`" }
        }
    }
}
