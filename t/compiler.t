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
    'my $a = 1; { say my $a = 2 }', # declare+use in block is fine
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
    '$a'            => 'used but not declared',
    'my'            => 'could not parse',
    '$a; my $a'     => 'used but not declared',
    'my $a ='       => 'could not parse',
    '$a = 42'       => 'used but not declared',
    '42 = my $a'    => 'could not parse',
    '42 := my $a'   => 'could not parse',
    'say $a'        => 'used but not declared',
    'say $a; my $a' => 'used but not declared',
    '++42'          => 'could not parse',
    '{ my $a }; say $a'   => 'used but not declared',
    'else { 42 }'         => 'could not parse',
    'if 42 say 42'        => 'could not parse',
    'if $a {}'            => 'could not parse',
    'if 42 { $a }'        => 'used but not declared',
    'if 5 {} else { $a }' => 'used but not declared',
    'unless {}'           => 'could not parse',
    'unless a {}'         => 'could not parse',
    'my $a = 1; { say $a; my $a = 2 }' => 'reference to outer variable',
;

for @programs-that-don't-compile -> $pair { # '
    my ($program, $expected-error) = .key, .value given $pair;
    my $can-compile = False;
    try {
        $c.compile($program);
        $can-compile = True;
    }
    ok !$can-compile && defined $!.substr($expected-error),
        "'{escape $program}' gives error '$expected-error'";
}

done;
