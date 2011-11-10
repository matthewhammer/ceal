#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use POSIX;
use Getopt::Std;
use Testutils;

my $targetdir; # <- Command-line arg

sub doopts {
    my %args;
    getopts("d:", \%args);
    die "required arg: -d <target-dir>\n" if (!exists $args{d});
    $targetdir = $args{d};
}

doopts();

my @apps = Testutils::apps_in_path($targetdir);

sub combine_2_columns {
    my $path = shift;
    my $col_name_1 = shift;
    my $col_name_2 = shift;
    my $combine_op = shift;
    my $col_name_3 = shift;
    
    open (COL1, "< $path/$col_name_1");
    open (COL2, "< $path/$col_name_2");
    open (COL3, "> $path/$col_name_3");

    my @col_1 = (<COL1>);
    my @col_2 = (<COL2>);
    
    my $idx_1 = 0;
    my $idx_2 = 0;

    while(exists($col_1[$idx_1]) && 
          exists($col_2[$idx_2])) {

        my $data_1 = $col_1[$idx_1]; chomp($data_1);
        my $data_2 = $col_2[$idx_2]; chomp($data_2);
        
        if(Testutils::entry_iscomment $data_1)
        { $idx_1 ++; next; }
        
        if(Testutils::entry_iscomment $data_2)
        { $idx_2 ++; next; }
        
        my $data_3 = $combine_op->($data_1, $data_2);
        
        print COL3 "## $data_1 / $data_2\n$data_3\n";
        
        $idx_1 ++;
        $idx_2 ++;
    }

    close (COL3);
}

sub divide {
    my $x = shift;
    my $y = shift;
    return $x / $y;
}

for my $app (@apps) {
    
    combine_2_columns("$targetdir/$app", 
                      "fromscratch/time", 
                      "verifier/time",
                      \&divide,
                      "fromscratch/overhead");                      

    combine_2_columns("$targetdir/$app", 
                      "verifier/time",
                      "allinsrem/time-ave",
                      \&divide,
                      "allinsrem/speedup");
}
