#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use Getopt::Std;
use Testutils;
use CollectSample;

my $targetdir;
my $command;

my %args;
getopts("d:c:", \%args);

die "required arg: -d <target-dir>\n" if !(exists $args{d});
die "required arg: -c <command-string>\n" if !(exists $args{c});

$targetdir = $args{d};
$command = $args{c};

CollectSample::collect_sample($targetdir, $command);

