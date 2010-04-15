use v6;

use Test;
plan *;

use Yapsi;
my Yapsi::Compiler $c .= new;

my @programs-that-compile =
    '',
    ';',
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
;

for @programs-that-compile -> $program {
    my $can-compile = False;
    try {
        $c.compile($program);
        $can-compile = True;
    }
    ok $can-compile, "will compile '$program'";
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
;

for @programs-that-don't-compile -> $program { # '
    my $can-compile = False;
    try {
        $c.compile($program);
        $can-compile = True;
    }
    ok !$can-compile, "will not compile '$program'";
}

done_testing;
