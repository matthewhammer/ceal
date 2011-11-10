#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use Getopt::Std;
use Testutils;

my $file_in;     # <- Command-line arg
my $file_out;    # <- Command-line arg
my $num_samples; # <- Command-line arg


sub doopts {
    my %args;
    getopts("s:f:o:", \%args);
    die "required arg: -f <file>\n" if (!exists $args{f});
    die "required arg: -o <file>\n" if (!exists $args{f});
    
    $file_in = $args{f};
    $file_out = $args{o};

    if(exists $args{s}) {
        $num_samples = $args{s};
    }
    else {
        $num_samples = 1;
    }
}

doopts();
Testutils::resample_file($file_in, $file_out, $num_samples);
