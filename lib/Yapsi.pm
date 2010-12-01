use v6;

my $VERSION = '2010.12';

my $_PROGRAM; # RAKUDO: Part of workaround required because of [perl #76894]

# At any given time, @blockstack contains the chain of blocks we're in,
# from the outermost (the main program body), to the innermost. Each block
# is stored as a hash, with the keys :name :vars, though :vars is only
# filled in at the time of the action method at the end of the parsing of
# the block.
my @blockstack;

grammar Yapsi::Perl6::Grammar {
    regex TOP { ^
                { push @blockstack, { :name(unique-block()) } }
                <statementlist> <.ws> $ }
    regex statementlist { <statement> ** <eat_terminator> }
    token statement { <statement_control> || <expression> || '' }
    # RAKUDO: <?after '}'> NYRI [perl #76894]
    regex eat_terminator { <?{ $/.CURSOR.pos > 0
                               && $_PROGRAM.substr($/.CURSOR.pos - 1, 1) eq '}'
                           }> \n
                           || <.ws> ';' }
    token expression { <assignment> || <binding> || <variable> || <literal>
                       || <declaration> || <invocation> || <block>
                       || <saycall> || <increment> || <decrement> }
    token statement_control { <statement_control_if>
                              || <statement_control_while_until> 
                              || <statement_control_unless> }
    rule  statement_control_if { 'if' <expression> <block>
                                 [ 'else' <else=.block> ]? }
    rule  statement_control_while_until { $<keyword>=[ 'while' | 'until' ] <expression> <block> }
    rule statement_control_unless { 'unless' <expression> <block> }
    token lvalue { <declaration> || <variable> || <increment> }
    token value { <variable> || <literal> || <declaration> || <saycall>
                  || <increment> }
    token variable { '$' \w+ }
    token literal { \d+ }
    rule  declaration { $<declarator>=['my'|'our'] <variable> }
    rule  assignment { <lvalue> '=' <expression> }
    rule  binding { <lvalue> ':=' <expression> }
    rule  saycall { 'say' <expression> }  # very temporary solution
    rule  increment { '++' <value> }
    rule  decrement { '--' <value> }
    rule  invocation { [<variable>||<block>]'()' }
    token block { <.ws> '{'
                  { push @blockstack, { :name(unique-block()) } }
                  <.ws> <statementlist> <.ws> '}' }
}

my $block-number = 0;   # Can be done with 'state' when Rakudo has it
sub unique-block() {
    'B' ~ $block-number++;
}

sub traverse-top-down(Match $m, :$key = "TOP", :&action, :@skip) {
    action($m, $key);
    for %($m).keys -> $key {
        next if $key eq any @skip;
        given $m{$key} {
            when Match { traverse-top-down($_, :$key, :&action, :@skip) }
            when Array { traverse-top-down($_, :$key, :&action, :@skip)
                            for .list }
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

    # At any given time, %vars maps each 'active' (currently lexically visible)
    # variables to the name of the block containing its outermost declaration.
    my %vars;

    # The reason we temporarily store 'declaration?' in %vars is that in a
    # bottom-up model such as the one the actions employ, <variable> fires
    # before <declaration>. Basically, there needs to be a way to say "this
    # may be an undeclared variable, or maybe a variable that is being
    # declared". The two possible cases of the former are caught in the
    # .variable and .block methods.

    method declaration($/) {
        if %vars{~$<variable>} eq 'declaration?' {
            %vars{~$<variable>} = @blockstack[*-1]<name>;
        }
    }

    method variable($/) {
        die qq[Variable "$/" used but not declared]
            if %vars{~$/} eq 'declaration?';
        unless %vars.exists(~$/) {
            %vars{~$/} = 'declaration?';
        }
    }

    method TOP($/) {
        self.block($/);
    }

    # As opposed to %vars with its parse-global reach, @vars only has meaning
    # within the .block method. It enumerates all of the variables declared
    # directly in the current block. We need to clone the Array each time we
    # store it, because we keep re-using the variable.
    my @vars;
    my &find-declarations = sub ($m, $key) {
        if $key eq "declaration" {
            push @vars, { :name(~$m<variable>),
                          :our($m<declarator> eq 'our') };
        }
    };

    method block($/) {
        @vars = ();
        traverse-top-down($/, :skip['block'], :action(&find-declarations));
        my $block = pop @blockstack;
        my $name = $block<name>;
        $block<vars> = @vars.clone;
        make $block;
        if @blockstack {
            %block-parents{$name} = @blockstack[*-1];
        }
        # RAKUDO: Can't assign to a hash when the hash is part of the rhs
        #         [perl #77586]
        my %workaround-vars = grep { .value ne $name }, %vars;
        %vars = %workaround-vars;
        if first { .value eq 'declaration?' }, %vars -> $p {
            die qq[Variable "$p.key()" used but not declared];
        }
    }

    method statement($/) {
        if $<expression> && $<expression><block> -> $e {
            my $block = $e.ast;
            $block<immediate> = "yes";
        }
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
        my %package-variables;
        my $*l = 0;         # unique label    counter
        traverse-top-down($/, :action(-> $m, $key {
            if $key eq 'TOP'|'block'|'else' {
                push @sic, '';
                push @sic, "block '$m.ast<name>':";
                for $m.ast<vars>.list -> $var {
                    push @sic, "    `var '$var<name>'"
                               ~ ($var<our> ?? ' :our' !! '');
                }
                my @blocksic;
                my $*c = 0; # unique register counter
                my @skip = 'block', 'statement_control_if',
                           'statement_control_while_until',
                           'statement_control_unless';
                my &sicify = -> $/, $key {
                    if $m !=== $/ && $key eq 'block' {
                        my $register = self.unique-register;
                        my $block-name = $/.ast<name>;
                        push @blocksic,
                            "$register = closure-from-block '$block-name'";
                        if $/.ast<immediate> {
                            push @blocksic, "call $register";
                        }
                        else {
                            $/.ast<register> = $register;
                        }
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
                    elsif $key eq 'statement_control_unless' {
                        traverse-bottom-up(
                            $<expression>,
                            :key<expression>,
                            :@skip,
                            :action(&sicify)
                        );
                        my ($register, $) = $<expression>.ast.list;
                        my $block-name = $<block>.ast<name>;
                        my $after-unless = self.unique-label;
                        push @blocksic, "jt $register, $after-unless";
                        $register = self.unique-register;
                        push @blocksic,
                            "$register = closure-from-block '$block-name'",
                            "call $register";
                        push @blocksic, "`label $after-unless";
                    }
                    elsif $key eq 'statement_control_while_until' {
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
                        given $<keyword> {
                            when / while / {
                                push @blocksic, "jf $register, $after-while";
                            }
                            when / until / {
                                push @blocksic, "jt $register, $after-while";
                            }
                        }
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
                                if ~$/ eq @vars[$i]<name> {
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
                        if $<block> {
                            make $<block>.ast<register>;
                        }
                    }
                    elsif $key eq 'literal' {
                        my $register = self.unique-register;
                        my $literal = ~$/;
                        push @blocksic, "$register = $literal";
                        make [$register, $literal];
                    }
                    elsif $key eq 'declaration' {
                        if $<declarator> eq 'our' {
                            ++%package-variables{~$<variable>};
                        }
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
                    elsif $key eq 'invocation' {
                        my ($register) = $<variable> ?? $<variable>.ast.list
                                                     !! $<block>.ast<register>;
                        push @blocksic, "call $register";
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
        if %package-variables {
            push @sic, '';
            push @sic, "block 'GLOBAL':";
            for %package-variables.keys -> $var {
                push @sic, "    `var '$var'";
            }
        }
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
    has Lexpad $.current-lexpad;

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

        my $global-lexpad;

        sub new-lexpad-from(@sic, $line is copy, Lexpad $outer?) {
            my (@vars, @slots);
            # RAKUDO: Some Any()s seem to end up in the @sic array. Hence the
            #         need for prefix:<~>. Would be interesting to learn where
            #         this happens.
            while ~@sic[++$line]
                    ~~ / '    `' (\S*) :s \'(<-[']>+)\' ( ':'\w+)* / {
                given $0 {
                    when "var" {
                        push @vars, ~$1;
                        my $is-our-variable = ?( ':our' eq any $2>>.Str );
                        my $container = $is-our-variable
                            ?? (.slots[.names{~$1}] given $global-lexpad)
                            !! Container.new;
                        push @slots, $container;
                    }
                    default { die "Unknown directive $0"; }
                }
            }
            return Lexpad.new(:@slots, :names((hash @vars.kv).invert), :$outer);
        }

        try {
            $global-lexpad
                = new-lexpad-from(@sic, find-block(@sic, 'GLOBAL'));
        }

        $!current-lexpad = new-lexpad-from(@sic, 2);
        self.?tick;
        my $ip = 3;
        while @registers-stack {
            while @sic[$ip++] -> $line {
                given $line.substr(4) {
                    when / ^ '`' / {}
                    when / ^ '$'(\d+) ' = ' (\d+) $ / { reg[+$0] = +$1 }
                    when / ^ 'store ['[(0)||'-'(\d+)]', '(\d+)'], $'(\d+) $ / {
                        my ($levels, $slot, $register) = +$0, +$1, +$2;
                        my $lexpad = n-up-from($!current-lexpad, $levels);
                        $lexpad.slots[$slot].store(
                            Value.new( :payload(reg[$register]) )
                        );
                        self.?tick;
                    }
                    when / ^ '$'(\d+)' = fetch '
                             '['[(0)||'-'(\d+)]', '(\d+)']' $ / {
                        my ($register, $levels, $slot) = +$0, +$1, +$2;
                        my $lexpad = n-up-from($!current-lexpad, $levels);
                        reg[$register] = $lexpad.slots[$slot].payload();
                    }
                    when / ^ 'bind ['[(0)||'-'(\d+)]', '(\d+)'], '
                                  '['[(0)||'-'(\d+)]', '(\d+)']' $ / {
                        my ($var1-levels, $var1-slot) = +$0, +$1;
                        my $var1-lexpad
                            = n-up-from($!current-lexpad, $var1-levels);
                        my ($var2-levels, $var2-slot) = +$2, +$3;
                        my $var2-lexpad
                            = n-up-from($!current-lexpad, $var2-levels);
                        $var1-lexpad.slots[$var1-slot]
                            = $var2-lexpad.slots[$var2-slot];
                        self.?tick;
                    }
                    when / ^ 'bind ['[(0)||'-'(\d+)]', '(\d+)'], '(\d+) $ / {
                        my ($levels, $slot, $literal) = +$0, +$1, +$2;
                        my $lexpad = n-up-from($!current-lexpad, $levels);
                        $lexpad.slots[$slot] = Value.new( :payload($literal) );
                        self.?tick;
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
                        self.?tick;
                    }
                    when / ^ 'jt $'(\d+)', '(\S+) $ / {
                        if reg[+$0] != 0 {
                            $ip = find-label(@sic, ~$1);
                        }
                        self.?tick;
                    }
                    when / ^ 'jmp '(\S+) $ / {
                        $ip = find-label(@sic, ~$0);
                        self.?tick;
                    }
                    when / ^ '$'(\d+)' = closure-from-block '
                             \'(<-[']>+)\' $ / {
                        reg[+$0] = Closure.new(:block(~$1),
                                               :outer($!current-lexpad));
                    }
                    when / ^ 'call $'(\d+) $ / {
                        die "Trying to call a non-closure"
                            if (my $closure = reg[+$0]) !~~ Closure;
                        push @registers-stack, [];
                        push @ip-stack, $ip;
                        $ip = find-block(@sic, $closure.block);
                        $!current-lexpad
                            = new-lexpad-from(@sic, $ip, $closure.outer);
                        ++$ip;
                        self.?tick;
                    }
                    when / ^ 'say $'(\d+) $ / {
                        $!io.say(reg[+$0]);
                        self.?tick;
                    }
                    default {
                        die "Unknown instruction: ", $_;
                    }
                }
            }
            pop @registers-stack;
            $ip = pop @ip-stack;
            $!current-lexpad.=outer;
        }
    }
}
