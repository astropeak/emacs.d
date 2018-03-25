$dir = $ARGV[0];
$file = $ARGV[1];


my @titles = ();
open my $fh, "<$dir/$file";
print "(\n";
while (<$fh>) {
    # increase line line
    ++$ln;
    s/[\r\n]//g;

    # remove all "
    s/"//g;

    if (/^(\*+)[ \t]+(.*)/) {
        # add parent
        my $level = length($1);
        # convert multiple spaces to one.
        my $val = $2;
        # print "before, val: $val|\n";
        $val =~ s/\s{2,}/ /g;
        # print "after    , val: $val|\n";

        $titles[$level] = $val;

        my $all = '';
        for (my $i=1;$i<=$level;$i++){
            $all .= $titles[$i];
            $all .= ' / ' if $i != $level;
        }

        print "(\"$file\" $ln \"$all\")\n";
    }
}
print ")\n";