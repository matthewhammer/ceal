#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use Getopt::Std;
use CollectSeries;
use CollectSample;

my $cealhome  = "../../";
my $targetdir;   # <- Command-line arg
my $batchmode;   # <- Command-line arg
my $num_samples; # <- Command-line arg


## Process command-line arguments
##
## mandatory: -d <target-dir>     Specify target directory for results
## mandatory: -s <num-samples>    Specify #samples at each input-size.
##  optional: -B                  Run in "batch-mode" (don't tail the log)

sub doopts {
    my %args;
    getopts("s:d:b:B", \%args);
    die "required arg: -d <target-dir>\n" if (!exists $args{d});
    die "required arg: -s <num-samples>\n" if (!exists $args{s});

    $targetdir = $args{d};
    $num_samples = $args{s};
    $batchmode = 1 if exists $args{B};
}

doopts();

# OLD
#Testutils::mysystem("cp $cealhome/slime_config.h $targetdir");

if(! $batchmode) {
    Testutils::mysystem("xterm -e \"tail -f $targetdir/LOG | grep -v '==\\W\\+Sys\\W\\+=='\" &");
}

Testutils::logg("Info", "$0: starting.");

my $requests = Testutils::get_requests_file($targetdir,
                                            "gather-data.local",
                                            "gather-data.defaults");

sub collect_series {
    
    my $app_name  = shift;
    my $app_ver   = shift;
    my $phases    = shift;
    my $min_n     = shift;
    my $max_n     = shift;
    
    my $binary    = "$cealhome/bin/apps/$app_name/$app_ver/$app_name";
    
    my $targetdir = "$targetdir/$app_name/$app_ver";

    my $command   = "$binary -phases $phases -srand \$R -input-size \$N";
    
    Testutils::logg("Info", "$0: $app_name $app_ver $phases $max_n: starting.");
    CollectSeries::collect_series($targetdir, $command, $min_n, $max_n, $num_samples);
}

## srv-53-08
## --- Plots, Foreign-{ring,splay}
if(0) {
    for my $line (<$requests>) {
        Testutils::logg("Info", "$0: read: $line");
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        
        {
            my $min_n = 10;
            collect_series($app_name, "foreign-ring",             "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-splay",            "ipvV",  $min_n, $max_n);
            collect_series($app_name, "verf",                     "v",     $min_n, $max_n);
        }
    }
}

## srv-53-010
## --- Bargraphs, Foreign-{ring,splay}
if(0) {    
    for my $line (<$requests>) {
        Testutils::logg("Info", "$0: read: $line");
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        
        {
            my $min_n = $max_n;
            collect_series($app_name, "foreign-ring",            "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-ring-no-seldps",  "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-ring-no-share",   "ipvV",  $min_n, $max_n);

            collect_series($app_name, "foreign-splay",            "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-splay-no-seldps",  "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-splay-no-share",   "ipvV",  $min_n, $max_n);

            collect_series($app_name, "verf",                    "v",     $min_n, $max_n);
        }
    }
}

## srv-53-013
## --- Plots, Implicit
if(0) {
    for my $line (<$requests>) {
        Testutils::logg("Info", "$0: read: $line");
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        
        {
            my $min_n = 10;
            collect_series($app_name, "implicit",              "ipvV",  $min_n, $max_n);
            collect_series($app_name, "verf",                     "v",  $min_n, $max_n);
        }
    }
}

## srv-53-014
## --- Bargraphs, Implicit (Part A).
if(0) {
    for my $line (<$requests>) {
        Testutils::logg("Info", "$0: read: $line");
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        
        {
            my $min_n = $max_n;
            collect_series($app_name, "implicit",                "ipvV",  $min_n, $max_n);
#            collect_series($app_name, "implicit-no-seldps",      "ipvV",  $min_n, $max_n);
#            collect_series($app_name, "implicit-no-share",       "ipvV",  $min_n, $max_n);
            collect_series($app_name, "implicit-no-opt",         "ipvV",  $min_n, $max_n);
        }
    }
}

## srv-53-015
## --- Bargraphs, Implicit (Part B).
if(0) {
    for my $line (<$requests>) {
        Testutils::logg("Info", "$0: read: $line");
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        
        {
            my $min_n = $max_n;
#            collect_series($app_name, "implicit",                "ipvV",  $min_n, $max_n);
            collect_series($app_name, "implicit-no-seldps",      "ipvV",  $min_n, $max_n);
            collect_series($app_name, "implicit-no-share",       "ipvV",  $min_n, $max_n);
#            collect_series($app_name, "implicit-no-opt",         "ipvV",  $min_n, $max_n);
        }
    }
}


## srv-53-014
## April 2 2011
## --- Bargraphs, Foreign-ring.
if(0) {
    for my $line (<$requests>) {
        Testutils::logg("Info", "$0: read: $line");
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        
        {
            my $min_n = $max_n;
            collect_series($app_name, "foreign-ring",            "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-ring-no-seldps",  "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreign-ring-no-share",   "ipvV",  $min_n, $max_n);
            collect_series($app_name, "foreing-ring-no-opt",     "ipvV",  $min_n, $max_n);
        }
    }
}

Testutils::logg("Info", "$0: complete.");
