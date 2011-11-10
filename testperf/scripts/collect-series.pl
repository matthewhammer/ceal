#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>

use strict;
use Getopt::Std;
use CollectSeries;

## Command-line "options" (all are mandatory)
my $targetdir;
my $command;
my $min_n;
my $max_n;

my %args;
getopts("d:c:n:m:", \%args);

die "required arg: -d <target-dir>\n" if !(exists $args{d});
die "required arg: -c <command-string>\n" if !(exists $args{c});
die "required arg: -n <min-n>\n" if !(exists $args{n});
die "required arg: -m <max-n>\n" if !(exists $args{m});

$targetdir = $args{d};
$command = $args{c};
$min_n = $args{n};
$max_n = $args{m};

CollectSeries::collect_series($targetdir, $command, $min_n, $max_n);

