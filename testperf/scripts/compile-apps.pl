#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use Testutils;
use Getopt::Std;

my $targetdir = ".";   # <- Command-line arg

## Process command-line arguments
##
## mandatory: -d <target-dir>     Specify target directory for results

sub doopts {
    my %args;
    getopts("d:", \%args);

    if (exists $args{d}) {
        $targetdir = $args{d};
    }
}

doopts();

my $requests = Testutils::get_requests_file($targetdir,
                                            "gather-data.local",
                                            "gather-data.defaults");

for my $line (<$requests>) {
    next if $line =~ /^\#/;
    my $min_n = 10;
    my ($app_name, $max_n) = Testutils::get_request_from_line $line;
    
    next if $app_name eq "";

    Testutils::mysystem("cd ../../build && ./compile-app.sh $app_name");
}

Testutils::logg("Info", "$0: complete.");
