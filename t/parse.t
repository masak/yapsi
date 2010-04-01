use v6;

use Test;
plan *;

use Yapsi;
my Yapsi::Compiler $c .= new;

my @programs-that-parse =
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
;

for @programs-that-parse -> $program {
    my $can-parse = False;
    try {
        $c.compile($program);
        $can-parse = True;
    }
    ok $can-parse, "will parse '$program'";
}

my @programs-that-don't-parse =   # '
    '$a',
    'my',
    '$a; my $a',
    'my $a =',
    '$a = 42',
    '42 = my $a',
    '42 := my $a',
    'say $a',
    'say $a; my $a',
;

for @programs-that-don't-parse -> $program { # '
    my $can-parse = False;
    try {
        $c.compile($program);
        $can-parse = True;
    }
    ok !$can-parse, "will not parse '$program'";
}

done_testing;
