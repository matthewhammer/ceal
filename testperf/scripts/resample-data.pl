#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use Getopt::Std;
use Testutils;
use File::Find;

my $target_dir;  # <- Command-line arg
my $num_samples; # <- Command-line arg

my $files = [];

sub doopts {
    my %args;
    getopts("d:s:", \%args);
    die "required arg: -d <target-dir>\n" if (!exists $args{d});    
    die "required arg: -s <num-samples>\n" if (!exists $args{s});

    $target_dir = $args{d};
    $num_samples = $args{s};
}

sub file_found {
    my $file = $File::Find::name;
    
    if($file =~ /((^.+)\.$num_samples)/) {
        my $file_in = $1;
        my $file_out = $2;

        next if $file =~ /\.svn/;

        chomp($file_in);
        chomp($file_out);
        my $file_pair = [$file_in, $file_out];        
        push @$files, $file_pair;
        Testutils::logg("Resample enqueue", "$file_in --> $file_out\n");
    }
}

sub resample {    
    for my $file_pair (@$files) {
        my ($file_in, $file_out) = @$file_pair;
        Testutils::logg("Resampling", "$file_in --> $file_out\n");
        Testutils::resample_file($file_in, $file_out, $num_samples);    
    }
}

doopts();
find(\&file_found, $target_dir);
resample();
