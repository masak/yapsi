use v6;

use Test;
plan *;

use Yapsi;
my Yapsi::Compiler $c .= new;

my @programs-that-compile =
    '',
    ';',
    '; ',
    '42',
    '42;',
    'my $a',
    'my $a;',
    'say 42',
    'my $a = 42;',
    'my $a; $a = 42;',
    'my $a; my $a; my $a',
    'my $a := 42; my $b = $a;',
    'my $a; my $b := $a; $a = 42',
    'my $a; say $a',
    'my $a; ++$a',
    '++my $a',
    '++my $a = 42',
    'my $a; {}; say $a',
    "my \$a; \{\}\nsay \$a",
    'my $a; { say $a }',
    'if 1 { say 42 }',
    'my $a; if $a {}',
    'if my $a {} else { say 42 }',
    'my $a; while $a { say $a }',
    'unless 0 { say 42 }',
    'my $a=0; unless $a { say $a }',
    'my $a=0; until $a { say 42; ++$a; }',
    'our $a',
    'my $a; $a()',
    '{ say 42 }()',
;

sub escape($string) { $string.subst("\n", "\\n", :g) }

for @programs-that-compile -> $program {
    my $can-compile = False;
    try {
        $c.compile($program);
        $can-compile = True;
    }
    ok $can-compile, "will compile '{escape($program)}'";
}

my @programs-that-don't-compile =   # '
    '$a',
    'my',
    '$a; my $a',
    'my $a =',
    '$a = 42',
    '42 = my $a',
    '42 := my $a',
    'say $a',
    'say $a; my $a',
    '++42',
    '{ my $a }; say $a',
    'else { 42 }',
    'if 42 say 42',
    'if $a {}',
    'if 42 { $a }',
    'if 5 {} else { $a }',
    'unless {}',
    'unless a {}',
;

for @programs-that-don't-compile -> $program { # '
    my $can-compile = False;
    try {
        $c.compile($program);
        $can-compile = True;
    }
    ok !$can-compile, "will not compile '$program'";
}

done;
