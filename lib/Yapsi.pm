use v6;

my $VERSION = '2010.08';

my $_PROGRAM; # RAKUDO: Part of workaround required because of [perl #76894]

grammar Yapsi::Perl6::Grammar {
    regex TOP { ^ <statementlist> <.ws> $ }
    regex statementlist { <statement> ** <eat_terminator> }
    token statement { <statement_control> || <expression> || '' }
    # RAKUDO: <?after '{'> NYRI [perl #76894]
    token eat_terminator { <?{ $/.CURSOR.pos > 1
                               && $_PROGRAM.substr($/.CURSOR.pos - 1, 1) eq "\{"
                           }> \n
                           || <.ws> ';' }
    token expression { <assignment> || <binding> || <variable> || <literal>
                       || <declaration> || <block>
                       || <saycall> || <increment> || <decrement> }
    token statement_control { <statement_control_if>
                              || <statement_control_while> }
    rule  statement_control_if { 'if' <expression> <block>
                                 [ 'else' <else=.block> ]? }
    rule  statement_control_while { 'while' <expression> <block> }
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
    rule  decrement { '--' <value> }
    token block { <.ws> '{' <.ws> <statementlist> <.ws> '}' }
}

my $block-number = 0;   # Can be done with 'state' when Rakudo has it
sub unique-block() {
    'B' ~ $block-number++;
}

sub descend-into(Match $m, :$key = "TOP", :&action, :@skip) {
    action($m, $key);
    for %($m).keys -> $key {
        next if $key eq any @skip;
        given $m{$key} {
            when Match { descend-into($_, :$key, :&action, :@skip) }
            when Array { descend-into($_, :$key, :&action, :@skip) for .list }
            default { die "Unknown thing $_.WHAT() in parse tree!" }
        }
    }
}

sub traverse-bottom-up(Match $m, :$key = "TOP", :&action, :@skip) {
    unless $key eq any @skip {
        for %($m).keys -> $key {
            given $m{$key} {
                when Match { traverse-bottom-up($_, :$key, :&action, :@skip) }
                when Array { traverse-bottom-up($_, :$key, :&action, :@skip)
                                for .list }
                default { die "Unknown thing $_.WHAT() in parse tree!" }
            }
        }
    }
    action($m, $key);
}

my %block-parents;

class Yapsi::Perl6::Actions {
    my @vars;
    my &find-declarations = sub ($m, $key) {
        if $key eq "declaration" {
            push @vars, ~$m<variable>;
        }
    };
    my &connect-blocks = sub ($name, $block, $m, $key) {
        if $key eq "block" && $m.ast<name> ne $name {
            %block-parents{$m.ast<name>} = $block;
        }
    };

    method TOP($/) {
        @vars = ();
        descend-into($/, :skip['block'], :action(&find-declarations));
        my $name = unique-block();
        make my $block = { :$name, :vars(@vars.clone) };
        descend-into($/, :action(&connect-blocks.assuming($name, $block)));
    }

    method block($/) {
        @vars = ();
        descend-into($/, :skip['block'], :action(&find-declarations));
        my $name = unique-block();
        make my $block = { :$name, :vars(@vars.clone) };
        descend-into($/, :action(&connect-blocks.assuming($name, $block)));
    }
}

class Yapsi::Compiler {
    has @.warnings;

    method compile($program) {
        @!warnings = ();
        $_PROGRAM = $program; # RAKUDO: Required because of [perl #76894]
        die "Could not parse"
            unless Yapsi::Perl6::Grammar.parse(
                        $program, :actions(Yapsi::Perl6::Actions));
        my @sic = "This is SIC v$VERSION";
        my $INDENT = '    ';
        descend-into($/, :action(-> $m, $key {
            if $key eq 'TOP'|'block'|'else' {
                push @sic, '';
                push @sic, "block '$m.ast<name>':";
                for $m.ast<vars>.list -> $var {
                    push @sic, "    `lexvar '$var'";
                }
                my @blocksic;
                my $*c = 0; # unique register counter
                my $*l = 0; # unique label    counter
                my @skip = 'block', 'statement_control_if',
                           'statement_control_while';
                my &sicify = -> $/, $key {
                    if $m !=== $/ && $key eq 'block' {
                        my $register = self.unique-register;
                        my $block-name = $/.ast<name>;
                        push @blocksic,
                            "$register = closure-from-block '$block-name'",
                            "call $register";
                    }
                    elsif $key eq 'statement_control_if' {
                        traverse-bottom-up(
                            $<expression>,
                            :key<expression>,
                            :@skip,
                            :action(&sicify)
                        );
                        my ($register, $) = $<expression>.ast.list;
                        my $block-name = $<block>.ast<name>;
                        my $after-if = self.unique-label;
                        push @blocksic, "jf $register, $after-if";
                        $register = self.unique-register;
                        push @blocksic,
                            "$register = closure-from-block '$block-name'",
                            "call $register";
                        my $after-else;
                        if $<else> {
                            $after-else = self.unique-label;
                            push @blocksic, "jmp $after-else";
                        }
                        push @blocksic, "`label $after-if";
                        if $<else> {
                            $block-name = $<else>[0].ast<name>;
                            $register = self.unique-register;
                            push @blocksic,
                                "$register = closure-from-block '$block-name'",
                                "call $register",
                                "`label $after-else";
                        }
                    }
                    elsif $key eq 'statement_control_while' {
                        my $before-while = self.unique-label;
                        my $after-while = self.unique-label;
                        push @blocksic, "`label $before-while";
                        traverse-bottom-up(
                            $<expression>,
                            :key<expression>,
                            :@skip,
                            :action(&sicify)
                        );
                        my ($register, $) = $<expression>.ast.list;
                        push @blocksic, "jf $register, $after-while";
                        my $block-name = $<block>.ast<name>;
                        $register = self.unique-register;
                        push @blocksic,
                            "$register = closure-from-block '$block-name'",
                            "call $register",
                            "jmp $before-while",
                            "`label $after-while";
                    }
                    elsif $key eq 'variable' {
                        my $register = self.unique-register;
                        my $current_block = $m.ast;
                        my $level = 0;
                        my $slot = -1;
                        while True {
                            my @vars = $current_block<vars>.list;
                            for ^@vars -> $i {
                                if ~$/ eq @vars[$i] {
                                    $slot = $i;
                                    # RAKUDO: Could use a 'last LOOP' here
                                    last;
                                }
                            }
                            last if $slot != -1;
                            --$level;
                            $current_block
                                = %block-parents{$current_block<name>};
                            die "Variable '$/' not declared"
                                unless defined $current_block;
                        }
                        my $locator = "[$level, $slot]";
                        push @blocksic, "$register = fetch $locator";
                        make [$register, $locator];
                    }
                    elsif $key eq 'assignment' {
                        my ($register, $) = $<expression>.ast.list;
                        my ($, $locator) = $<lvalue>.ast.list;
                        push @blocksic, "store $locator, $register";
                        make [$register, $locator];
                    }
                    elsif $key eq 'binding' {
                        my ($, $leftloc) = $<lvalue>.ast.list;
                        my ($register, $rightloc) = $<expression>.ast.list;
                        push @blocksic, "bind $leftloc, $rightloc";
                        make [$register, $leftloc];
                    }
                    elsif $key eq 'value' {
                        for <variable literal declaration saycall
                             increment decrement> -> $e {
                            if $/{$e} {
                                make $/{$e}.ast;
                            }
                        }
                    }
                    elsif $key eq 'lvalue' {
                        for <variable declaration increment decrement> -> $e {
                            if $/{$e} {
                                make $/{$e}.ast;
                            }
                        }
                    }
                    elsif $key eq 'expression' {
                        for <variable literal declaration assignment binding
                             saycall increment decrement> -> $e {
                            if $/{$e} {
                                make $/{$e}.ast;
                            }
                        }
                    }
                    elsif $key eq 'literal' {
                        my $register = self.unique-register;
                        my $literal = ~$/;
                        push @blocksic, "$register = $literal";
                        make [$register, '<constant>'];
                    }
                    elsif $key eq 'declaration' {
                        make $<variable>.ast;
                    }
                    elsif $key eq 'increment' {
                        my ($register, $locator) = $<value>.ast.list;
                        die "Can't increment a constant"
                            if $locator eq '<constant>';
                        push @blocksic, "inc $register",
                                        "store $locator, $register";
                        make [$register, $locator];
                    }
                    elsif $key eq 'decrement' {
                        my ($register, $locator) = $<value>.ast.list;
                        die "Can't increment a constant"
                            if $locator eq '<constant>';
                        push @blocksic, "dec $register",
                                        "store $locator, $register";
                        make [$register, $locator];
                    }
                    elsif $key eq 'saycall' {
                        my ($register, $) = $<expression>.ast.list;
                        my $result = self.unique-register;
                        push @blocksic, "say $register",
                                        "$result = 1";
                        make $result;
                    }
                };
                traverse-bottom-up($m, :@skip, :action(&sicify));
                for renumber declutter @blocksic {
                    push @sic, $INDENT ~ $_;
                }
            }
        }));
        return @sic;
    }

    method unique-register {
        return '$' ~ $*c++;
    }

    method unique-label {
        return 'L' ~ $*l++;
    }

    sub declutter(@instructions) {
        my @decluttered;
        for @instructions.kv -> $i, $line {
            # RAKUDO: !~~ doesn't bind $/
            if not $line ~~ / ^ ('$' \d+) ' =' / {
                push @decluttered, $line;
            }
            else {
                my $varname = ~$0;
                my Bool $usages-later = False;
                for $i+1 ..^ +@instructions -> $j {
                    # XXX: This heuristic fails when we reach many-digit
                    #      reguster names, since it gives false positives
                    #      for all prefixes
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
        # RAKUDO: $/ doesn't work in .subst closures
        my $hack;
        return @instructions.map: {
            .subst( :global, / ('$' \d+) { $hack = ~$0 } /, {
                my $varname = $hack;
                if !%mapping.exists($varname) {
                    %mapping{$varname} = '$' ~ $number++;
                }
                %mapping{$varname}
            } );
        };
    }
}

class Value {
    has $.payload;

    method store($v) { die "Can't assign to a readonly value" }
}

class Container {
    has Value $!value;

    method store(Value $v) { $!value = $v }
    method fetch() { $!value }
    method payload() { $!value.defined ?? $!value.payload !! "Any()" }
}

class Lexpad {
    has @.slots;
    has %.names;
    has Lexpad $.outer;

    method Str {
        "lexpad[" ~ %.names.sort(*.value)>>.key.join(", ") ~ "]";
    }
}

class Closure {
    has $.block;
    has Lexpad $.outer;
}

sub new-lexpad-from(@sic, $line is copy, Lexpad $outer?) {
    my @lexvars;
    while @sic[++$line] ~~ / '    `' (\S*) \s+ \'(<-[']>+)\' / {
        given $0 {
            when "lexvar" { push @lexvars, ~$1 }
            default { die "Unknown directive $0"; }
        }
    }
    return Lexpad.new(:slots(map { Container.new }, ^@lexvars),
                      :names((hash @lexvars.kv).invert),
                      :$outer);
}

sub find-block(@sic, $name) {
    for @sic.kv -> $n, $line {
        return $n
            if $line ~~ / ^ 'block '\'(<-[']>+)\'':' $ / && $0 eq $name;
    }
    die "Didn't find block $name";
}

sub find-label(@sic, $name) {
    for @sic.kv -> $n, $line {
        return $n
            if $line ~~ / ^ '    `label '(\S+) $ / && $0 eq $name;
    }
    die "Didn't find label $name";
}

subset Yapsi::IO where { .can('say') }

class Yapsi::Runtime {
    has Yapsi::IO $!io = $*OUT;

    method run(@sic) {
        # RAKUDO: Need to use 'not' here rather than '!~~' [perl #76892]
        if not @sic[0] ~~ /^ 'This is SIC v'(\d\d\d\d\.\d\d) $/ {
            die "Incompatible SIC version line";
        }
        elsif ~$0 ne $VERSION {
            die "SIC is $0 but this is $VERSION -- cannot run";
        }

        my @registers-stack = [];
        my @ip-stack;

        sub reg() { @registers-stack[@registers-stack - 1] }
        sub n-up-from($lexpad is copy, $levels) {
            $lexpad.=outer for ^$levels;
            die "Went too far and ended up nowhere"
                unless defined $lexpad;
            $lexpad;
        }

        my $current-lexpad = new-lexpad-from(@sic, 2);
        my $ip = 3;
        while @registers-stack {
            while @sic[$ip++] -> $line {
                given $line.substr(4) {
                    when / ^ '`' / {}
                    when / ^ '$'(\d+) ' = ' (\d+) $ / { reg[+$0] = +$1 }
                    when / ^ 'store ['[(0)||'-'(\d+)]', '(\d+)'], $'(\d+) $ / {
                        my ($levels, $slot, $register) = +$0, +$1, +$2;
                        my $lexpad = n-up-from($current-lexpad, $levels);
                        $lexpad.slots[$slot].store(
                            Value.new( :payload(reg[$register]) )
                        );
                    }
                    when / ^ '$'(\d+)' = fetch ['[(0)||'-'(\d+)]', '(\d+)']' $ / {
                        my ($register, $levels, $slot) = +$0, +$1, +$2;
                        my $lexpad = n-up-from($current-lexpad, $levels);
                        reg[$register] = $lexpad.slots[$slot].payload();
                    }
                    when / ^ 'bind ['[(0)||'-'(\d+)]', '(\d+)'], '
                                  '['[(0)||'-'(\d+)]', '(\d+)']' $ / {
                        my ($var1-levels, $var1-slot) = +$0, +$1;
                        my $var1-lexpad = n-up-from($current-lexpad, $var1-levels);
                        my ($var2-levels, $var2-slot) = +$2, +$3;
                        my $var2-lexpad = n-up-from($current-lexpad, $var2-levels);
                        $var1-lexpad.slots[$var1-slot] = $var2-lexpad.slots[$var2-slot];
                    }
                    when / ^ 'inc $'(\d+) $ / {
                        reg[+$0] = reg[+$0] eq 'Any()' ?? 1 !! reg[+$0] + 1;
                    }
                    when / ^ 'dec $'(\d+) $ / {
                        reg[+$0] = reg[+$0] eq 'Any()' ?? 1 !! reg[+$0] - 1;
                    }
                    when / ^ 'jf $'(\d+)', '(\S+) $ / {
                        if reg[+$0] == 0 {
                            $ip = find-label(@sic, ~$1);
                        }
                    }
                    when / ^ 'jmp '(\S+) $ / {
                        $ip = find-label(@sic, ~$0);
                    }
                    when / ^ '$'(\d+)' = closure-from-block '\'(<-[']>+)\' $ / {
                        reg[+$0] = Closure.new(:block(~$1), :outer($current-lexpad));
                    }
                    when / ^ 'call $'(\d+) $ / {
                        die "Trying to call a non-closure"
                            if (my $closure = reg[+$0]) !~~ Closure;
                        push @registers-stack, [];
                        push @ip-stack, $ip;
                        $ip = find-block(@sic, $closure.block);
                        $current-lexpad = new-lexpad-from(@sic, $ip, $closure.outer);
                        ++$ip;
                    }
                    when / ^ 'say $'(\d+) $ / {
                        $!io.say(reg[+$0]);
                    }
                    default {
                        die "Unknown instruction: ", $_;
                    }
                }
            }
            pop @registers-stack;
            $ip = pop @ip-stack;
            $current-lexpad.=outer;
        }
    }
}
