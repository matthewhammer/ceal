#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use POSIX;
use Getopt::Std;
use Testutils;
use Template;
use File::Basename;
use POSIX qw(log10);
use Dataset;

my $targetdir; # <- Command-line arg

sub doopts {
    my %args;
    getopts("d:", \%args);
    die "required arg: -d <target-dir>\n" if (!exists $args{d});
    $targetdir = $args{d};
}

doopts();

my $requests = Testutils::get_requests_file($targetdir,
                                            "plot-data.local",
                                            "plot-data.defaults");

my $dataset = Dataset::load($targetdir, $requests);

sub bargraph_from_template {    
    my $template = shift;
    
    ## How do we find the template files?
    my $tt = Template->new({
    INCLUDE_PATH => '../bargraph_templates',
                       }) || die "$Template::ERROR\n";
    
    ## Where do the bargraphs go?
    my $bargraphs_dir = "$targetdir/bargraphs";

    my $vars = {
        isverf => sub { my $x=shift; return 1 if $x eq "verf"; return 0; },
        apps   => $dataset->{apps},
        bins   => $dataset->{bins},
    };
    
    ## The bargraph file from the template
    my $bargraph_file = $template;
    $bargraph_file =~ s/\.template$//;
   
    ## Create the directory for the bargraphs, if it doesn't exist.
    Testutils::mysystem("mkdir -p $bargraphs_dir");
    
    ## Process the bargraph template into a bargraph spec ready for bargraph.pl
    $tt->process($template, $vars, "$bargraphs_dir/$bargraph_file")
        || do {
            my $error = $tt->error();
            print "error type: ", $error->type(), "\n";
            print "error info: ", $error->info(), "\n";
            die;
    };

    my $psfile = $bargraph_file;
    $psfile =~ s/\.bargraph$//;
    $psfile = "$psfile.ps";

    Testutils::mysystem("./bargraph.pl $bargraphs_dir/$bargraph_file ".
                        " > $bargraphs_dir/$psfile");
}

bargraph_from_template("fromscratch.bargraph.template");
bargraph_from_template("allinsrem.bargraph.template");
bargraph_from_template("maxlive.bargraph.template");
bargraph_from_template("speedup.bargraph.template");
bargraph_from_template("overhead.bargraph.template");
