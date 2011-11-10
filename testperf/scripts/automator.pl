#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
#
# See README for documentation on usage.

use strict;
use POSIX;
use Getopt::Std;
use Testutils;

my $targetdir; # <- Command-line arg
my $num_samples; # <- Command-line arg

my $do_gather = 0;
my $do_force = 0;

sub doopts {
    my %args;
    getopts("d:s:gf", \%args);
    die "required arg: -d <target-dir>\n" if (!exists $args{d});
    die "required arg: -s <num-samples>\n" if (!exists $args{s});
    
    $targetdir = $args{d};
    $num_samples = $args{s};
    
    $do_gather = 1 if(exists $args{g});
    $do_force = 1 if(exists $args{f});
}


sub main {
    doopts();
    
    ## Should we gather data?
    if ($do_gather)  {
        
        ## Does some data already exist?
        if(!( -d $targetdir) || $do_force) {

            if($do_force) {
                Testutils::mysystem("rm -rf $targetdir");
            }
	    
	    ## Create the target directory
	    Testutils::mysystem("mkdir -p $targetdir");
	    
            open(STDERR, ">$targetdir/LOG") or 
                die "couldn't open log file for writing";

            ## Compile the Apps
            Testutils::mysystem("./compile-apps.pl -d $targetdir");

            ## Gather the data.
            Testutils::mysystem("./gather-data.pl -B -s $num_samples -d $targetdir");
              
            ## Get some machine specs.
            Testutils::mysystem("./report-machine.pl    > $targetdir/specs.txt");
            Testutils::mysystem("./report-machine.pl -t > $targetdir/specs.tex");          
        }
        else {
            die "already exists: $targetdir, use -f to override";
        }
    }

    ## Resample the data.
    Testutils::mysystem("./resample-data.pl -s $num_samples -d $targetdir");

    ## Compute some additional data.
    Testutils::mysystem("./postprocess-data.pl -d $targetdir");
    
    ## Write a summarization table.
#    Testutils::mysystem("./summarize-data.pl -d $targetdir");

    ## Create plots of the data (as EPS files).
    Testutils::mysystem("./plot-data.pl -d $targetdir");

    ## Generate PDFs for each EPS plot.
    Testutils::mysystem("./epstopdf_all.sh $targetdir");

    ## Done.
    Testutils::logg("Info", "$0: done.");
}

main();
