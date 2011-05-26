use v6;

use Test;
use Yapsi;
my $c = ::Yapsi::Compiler.new;

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
    'sub foo {}',
    'my sub foo {}',
    'our sub foo {}',
    'sub foo {}; foo',
    'sub foo {}; foo()',
    'say 2; ENTER { say 1 }'
;

sub escape($string) { $string.subst(/\n/, "\\n", :g) }

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
    'my'            => 'Malformed my',
    '$a; my $a'     => 'used but not declared',
    'my $a ='       => 'Could not parse',
    '$a = 42'       => 'used but not declared',
    '42 = my $a'    => 'Could not parse',
    '42 := my $a'   => 'Could not parse',
    'say $a'        => 'used but not declared',
    'say $a; my $a' => 'used but not declared',
    '{ my $a }; say $a'   => 'used but not declared',
    'else { 42 }'         => 'Could not parse',
    'if 42 say 42'        => 'Could not parse',
    'if $a {}'            => 'used but not declared',
    'if 42 { $a }'        => 'used but not declared',
    'if 5 {} else { $a }' => 'used but not declared',
    'unless {}'           => 'Could not parse',
    'unless a {}'         => 'used but not declared',
#   TODO
#    'my $a = 1; { say $a; my $a = 2 }' => 'reference to outer variable',
    'sub foo'             => 'Could not parse',
    'foo'                 => 'used but not declared',
;

for @programs-that-don't-compile -> $pair { # '
    my ($program, $expected-error) = .key, .value given $pair;
    my $can-compile = False;
    try {
        $c.compile($program);
        $can-compile = True;
    }
    ok !$can-compile && defined($!.index($expected-error)),
        "'{escape $program}' gives error '$expected-error'";
}

done;
