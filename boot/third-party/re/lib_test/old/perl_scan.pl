$s = 'a' x (1024 * 1024 - 1) . 'b';
for ($i = 1; $i < 100; $i++) {
print "$&\n" if $s =~ /aa?b/ ;
}
