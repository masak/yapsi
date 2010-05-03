use v6;

my $VERSION = '2010.06';

grammar Yapsi::Perl6::Grammar {
    regex TOP { ^ <statementlist> <.ws> $ }
    regex statementlist { <statement> ** <eat_terminator> }
    token statement { <expression> || '' }
    token eat_terminator { <?after '}'> \n || <.ws> ';' }
    token expression { <assignment> || <binding> || <variable> || <literal>
                       || <declaration> || <saycall> || <increment>
                       || <block> }
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
    token block { <.ws> '{' <.ws> <statementlist> <.ws> '}' }
}

class Yapsi::Environment {
    has %.pads;
    has @.containers;
}

class Yapsi::Compiler {
    has @.warnings;

    method compile($program, Yapsi::Environment :$env) {
        @!warnings = ();
        die "Could not parse"
            unless Yapsi::Perl6::Grammar.parse($program);
        my Yapsi::Environment $*env
            = $env ~~ Yapsi::Environment
                && defined $env
                ?? $env !! Yapsi::Environment.new;
        my $*current-block = '';
        my @*block-counters; # keeps track of nested block numbers
        self.find-vars($/, 'block');
        my @sic = "This is SIC v$VERSION", '', 'environment:';
        my $INDENT = '    ';
        for $*env.pads.keys.sort -> $pad {
            push @sic, $INDENT ~ $pad ~ ':';
            for $*env.pads{$pad}.keys -> $var {
                push @sic, $INDENT x 2 ~ $var ~ ': '
                           ~ $*env.pads{$pad}{$var}.perl;
            }
        }
        push @sic, $INDENT ~ 'containers: ' ~ $*env.containers.perl;
        my $*c = 0; # unique register counter; increases with each new register
        my @*block-order;
        my %*blocks;
        $*current-block = '';
        self.sicify($/, 'block');
        for @*block-order -> $block {
            push @sic, '';
            push @sic, "block '$block':";
            for renumber(declutter(%*blocks{$block})) {
                push @sic, $INDENT ~ $_;
            }
        }
        return @sic;
    }

    multi method find-vars(Match $/, 'statement') {
        # RAKUDO: Autovivification
        if $<expression> && $<expression><block> -> $e {
            my $remember-block = $*current-block;
            self.find-vars($e, 'block');
            $*current-block = $remember-block;
        }
        elsif $<expression> -> $e {
            self.find-vars($e, 'expression');
        }
    }

    multi method find-vars(Match $/, 'expression') {
        # XXX: This warning doesn't have much to do with finding vars
        if $/<block> {
            die "Can not handle non-immediate blocks yet. Sorry. :/";
        }
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

    multi method find-vars(Match $name, 'variable') {
        my $block = $*current-block;
        loop {
            return if $*env.pads{$block}.exists( ~$name );
            last unless $block ~~ / _\d+ $/;
            $block.=substr(0, $block.chars - $/.chars);
        }
        die "Invalid. $name not declared before use";
    }

    multi method find-vars(Match $/, 'literal') {
        die "This multi variant should never be called";
    }

    multi method find-vars(Match $/, 'declaration') {
        my $name = ~$<variable>;
        if $*env.pads{$*current-block}{$name} {
            @!warnings.push: "Useless redeclaration of variable $name";
        }
        else {
            $*env.pads{$*current-block}{$name}
                = { :type<container>, :n(+$*env.containers) };
            push $*env.containers, 'Any()';
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

    multi method find-vars(Match $/, 'block') {
        if $*current-block {
            $*current-block ~= '_' ~ @*block-counters[*-1]++;
            push @*block-counters, 1;
        }
        else {
            $*current-block = 'main';
            @*block-counters = 1;
        }
        $*env.pads{$*current-block} //= {};
        for $<statementlist><statement> -> $statement {
            self.find-vars($statement, 'statement');
        }
        pop @*block-counters;
    }

    multi method find-vars($/, $node) {
        die "Don't know what to do with a $node";
    }

    method unique-register {
        return '$' ~ $*c++;
    }

    method add-code($line) {
        %*blocks{$*current-block}.push($line);
    }

    multi method sicify(Match $/, 'statement') {
        # RAKUDO: Autovivification
        if $<expression> && $<expression><block> -> $e {
            my $remember-block = $*current-block;
            my $block = self.sicify($e, 'block');
            my $register = self.unique-register;
            $*current-block = $remember-block;
            self.add-code: "$register = fetch-block '$block'";
            self.add-code: "call $register";
        }
        elsif $<expression> -> $e {
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
        self.add-code: "$register = fetch $variable";
        return ($register, $variable);
    }

    multi method sicify(Match $/, 'literal') {
        my $register = self.unique-register;
        my $literal = ~$/;
        self.add-code: "$register = $literal";
        return ($register, '<constant>');
    }

    multi method sicify(Match $/, 'declaration') {
        return self.sicify($<variable>, 'variable');
    }

    multi method sicify(Match $/, 'assignment') {
        my ($register, $) = self.sicify($<expression>, 'expression');
        my ($, $variable) = self.sicify($<lvalue>, 'lvalue');
        self.add-code: "store $variable, $register";
        return ($register, $variable);
    }

    multi method sicify(Match $/, 'binding') {
        my ($register, $rightvar) = self.sicify($<expression>, 'expression');
        my ($, $leftvar) = self.sicify($<lvalue>, 'lvalue');
        if $rightvar ~~ / ^ \d+ $ / { # hm. this is brittle and suboptimal.
            $rightvar = $register;
        }
        self.add-code: "bind $leftvar, $rightvar";
        return ($register, $leftvar);
    }

    multi method sicify(Match $/, 'saycall') {
        my ($register, $) = self.sicify($<expression>, 'expression');
        my $result = self.unique-register;
        self.add-code: "say $register";
        self.add-code: "$result = 1";
        return ($result, 1);
    }

    multi method sicify(Match $/, 'increment') {
        my ($register, $variable) = self.sicify($<value>, 'value');
        die "Can't increment a constant"
            if $variable eq '<constant>';
        self.add-code: "inc $register";
        self.add-code: "store $variable, $register";
        return ($register, $variable);
    }

    multi method sicify(Match $/, 'block') {
        if $*current-block {
            $*current-block ~= '_' ~ @*block-counters[*-1]++;
            push @*block-counters, 1;
        }
        else {
            $*current-block = 'main';
            @*block-counters = 1;
        }
        @*block-order.push($*current-block);
        %*blocks{$*current-block} = [];
        for $<statementlist><statement> -> $statement {
            self.sicify($statement, 'statement');
        }
        pop @*block-counters;
        return $*current-block;
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
    has Yapsi::Environment $.env;

    has $!current-block;

    method run(@sic) {
        if @sic[0] !~~ /^ 'This is SIC v'(\d\d\d\d\.\d\d) $/ {
            die "Incompatible SIC version line";
        }
        elsif ~$0 ne $VERSION {
            die "SIC is $0 but this is $VERSION -- cannot run";
        }
        {
            $!env = Yapsi::Environment.new;
            my $line = 3;
            my $block;
            while @sic[$line++] -> $decl {
                if $decl ~~ /^ '    containers: ' (.+) $/ {
                    $!env.containers.push($_) for eval(~$0).list;
                }
                elsif $decl ~~ /^ '        ' (<-[:]>+) ': ' (.+) $/ {
                    $!env.pads{$block}{~$0} = eval(~$1);
                }
                elsif $decl ~~ /^ '    ' (<-[:]>+) ':' $/ {
                    $block = ~$0;
                    $!env.pads{$block} //= {};
                }
                else {
                    die "Unknown environment declaration `$decl`";
                }
            }
        }
        my @r;
        $!current-block = 'main';
        my $ip = find-block(@sic, $!current-block) + 1;
        my @stack;
        self.*tick;
        loop {
            if $ip >= @sic || @sic[$ip] eq '' {
                return unless @stack;
                $ip = pop @stack;
                $!current-block .= substr(0, -2);
                redo;
            }
            given @sic[$ip++].substr(4) {
                when /^ '$'(\d+) ' = ' (\d+) $/ {
                    @r[+$0] = +$1
                }
                when /^ 'store ' \'(<-[']>+)\' ', $'(\d+) $/ {
                    my $thing = locate-variable($!env.pads, $!current-block, ~$0);
                    if $thing<type> eq 'container' {
                        my $n = $thing<n>;
                        $!env.containers[$n] = @r[+$1];
                    }
                    else {
                        die "Cannot store something in readonly symbol ~$0";
                    }
                    self.*tick;
                }
                when /^ '$'(\d+) ' = fetch '\'(<-[']>+)\' $/ {
                    @r[+$0] = self.get-value-of(~$1);
                }
                when /^ 'bind ' \'(<-[']>+)\' ', ' \'(<-[']>+)\' $/ {
                    $!env.pads{$!current-block}{~$0} = $!env.pads{$!current-block}{~$1};
                    self.*tick;
                }
                when /^ 'bind ' \'(<-[']>+)\' ', $'(\d+) $/ {
                    $!env.pads{$!current-block}{~$0}
                        = { :type<immediate>, :value(+$1) };
                }
                when /^ 'say $'(\d+) $/ {
                    $!io.say: @r[+$0];
                    self.*tick;
                }
                when /^ 'inc $'(\d+) $/ {
                    if @r[+$0] eq 'Any()' {
                        @r[+$0] = 1;
                    }
                    else {
                        ++@r[+$0];
                    }
                }
                when /^ '$'(\d+) ' = fetch-block '\'(<-[']>+)\' $/ {
                    @r[+$0] = ~$1;
                }
                when /^ 'call $'(\d+) $/ {
                    push @stack, $ip;
                    $ip = find-block(@sic, @r[+$0]) + 1;
                    $!current-block = @r[+$0];
                }
                default { die "Couldn't handle instruction `$_`" }
            }
        }
    }

    sub find-block(@sic, Str $block-sought) {
        for ^@sic {
            if @sic[$_] ~~ /^'block ' \'(<-[']>+)\'/ && ~$0 eq $block-sought {
                return $_;
            }
        }
        die "Could not find block '$block-sought'";
    }

    sub locate-variable(%pads, $block is copy, Str $name) {
        loop {
            return %pads{$block}{$name}
                if %pads{$block}.exists($name);
            last unless $block ~~ / _\d+ $/;
            $block.=substr(0, $block.chars - $/.chars);
        }
        die "Runtime panic -- could not find variable $name";
    }

    method get-value-of($variable) {
        my $thing = locate-variable($!env.pads, $!current-block, $variable);
        return $thing<type> eq 'container'
            ?? $!env.containers[$thing<n>]
            !! $thing<value>;
    }
}
