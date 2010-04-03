use v6;

use Test;
plan *;

use Yapsi;

my $out;
sub clear-out() { $out = '' }
my $io-collector = class { method say($i) {$out ~= $i ~ "\n"} };

my Yapsi::Compiler $compiler .= new;
my Yapsi::Runtime $runtime .= new( :io($io-collector) );

clear-out;
$runtime.run( $compiler.compile('say 42') );

is $out, "42\n", '"say 42" outputs "42\\n"';

done_testing;
