#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use POSIX;
use Getopt::Std;
use Testutils;
use Template;

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

## pick_xtics() --- GNUPLOT has many severe holes in its
## functionality.  One of them is allowing the user to say how many
## xtics they want without having to say how far apart they are along
## the x axis.  Basically I want to have xtics spaced out so that I
## can read each of them (they don't overlap eachother).  This
## "high-level" goal is reached by ensuring there are only 5 tics
## total.  I do this by picking the xtics (increment) value to be
## ($max_n / 5).  Also, since GNUPLOT can't format numbers in any
## useful way, I hand-format the values that label the xtics, e.g.,
## using notation like "100K" for the number 100,0000.
sub pick_xtics {
    my $max_n = shift;
    chomp($max_n);   
    my $incr = floor($max_n / 4);
    my $xtics = [];
    for(my $x = 0; $x <= $max_n; $x += $incr) {
        my $xlab = Testutils::format_number($x,"size_10");
        $xlab =~ s/(\.0)([KkMmGg]?)/$2/;
        push @$xtics, "\"$xlab\" $x";
    }    
    return ("(".(join(", ",@$xtics)).")");
}

sub nice_appname {
    my $name = shift;
    $name = ucfirst($name);
    $name =~ s/_/\\\\_/g;
    return $name;
}


sub plot_from_template {
    my $app_name = shift;
    my $max_n    = shift;
    my $template = shift;

    Testutils::logg("Info", "plot_from_template: $app_name $max_n $template");
    
    ## Where do we find the data?
    my $data_dir  = "$targetdir/$app_name/";

    ## Where do the plots go?
    my $plots_dir = "$targetdir/$app_name/plots";
    
    ## Get the plot filename by stripping ".template" from the end of
    ## the template filename.
    ## (The $plot_file goes in the $plots_dir).
    my $plot_file = $template;
    $plot_file =~ s/\.template$//;    
    
    ## How do we find the template files?
    my $tt = Template->new({
        INCLUDE_PATH => '../plot_templates/oopsla2011',
    }) || die "$Template::ERROR\n";
    
    ## Parameters to the template:
    my $vars = {
        wd         => `pwd`,
        app        => $app_name,
        appname    => nice_appname($app_name),
        max_x      => $max_n,
        xtics      => pick_xtics($max_n),
        data_dir   => $data_dir,
        output_dir => $plots_dir,
    };
    
    ## Create the directory for the plot-file and plots, if it doesn't exist.
    Testutils::mysystem("mkdir -p $plots_dir");
    
    ## Process the plot-file template into a plotfile ready for gnuplot.
    $tt->process($template, $vars, "$plots_dir/$plot_file")
    || do {
        my $error = $tt->error();
        print "error type: ", $error->type(), "\n";
        print "error info: ", $error->info(), "\n";
        print $error, "\n";
    };
    
    ## Run gnuplot on the plotfile.
    Testutils::mysystem("gnuplot $plots_dir/$plot_file");

}

for my $line (<$requests>) {
    next if $line =~ /^\#/;
    my ($app_name, $max_n) = Testutils::get_request_from_line $line;
    
    ## For OOPSLA 2011 : 
    plot_from_template($app_name, $max_n, "fromscratch.gnuplot.template");
    plot_from_template($app_name, $max_n, "allinsrem.gnuplot.template");
    plot_from_template($app_name, $max_n, "maxlive.gnuplot.template");
    plot_from_template($app_name, $max_n, "overhead.gnuplot.template");
    plot_from_template($app_name, $max_n, "speedup.gnuplot.template");
    #plot_from_template($app_name, $max_n, "trnode-revokes.gnuplot.template");

    # plot_from_template($app_name, $max_n, "fromscratch.gnuplot.template");
    # plot_from_template($app_name, $max_n, "allinsrem.gnuplot.template");
    # plot_from_template($app_name, $max_n, "maxlive.gnuplot.template");
    # plot_from_template($app_name, $max_n, "overhead.gnuplot.template");
    # plot_from_template($app_name, $max_n, "speedup.gnuplot.template");

    ## plot_from_template($app_name, $max_n, "steps-fromscratch.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "steps-allinsrem.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "steps-fromscratch-vs-aveupdate.gnuplot.template");
    
    ##
    ## plot_from_template($app_name, $max_n, "fromscratch.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "allinsrem.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "maxlive.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "overhead.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "speedup.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "memo-move.gnuplot.template");
    ## plot_from_template($app_name, $max_n, "memo-search.gnuplot.template");
}
