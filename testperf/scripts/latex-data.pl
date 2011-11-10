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
                                            "latex-data.local",
                                            "latex-data.defaults");

my $dataset = Dataset::load($targetdir, $requests);

sub latex_from_template {
    my $template = shift;
    
    ## How do we find the template files?
    my $tt = Template->new({
    INCLUDE_PATH => '../latex_templates',
                       }) || die "$Template::ERROR\n";

    ## Where does the latex go?
    my $latex_dir = "$targetdir/latex";


    sub format_input_size {
        my $n = shift;
        return "--" if ! $n;
        if( log10($n) == int(log10($n)) ) {
            return sprintf("\$10^%d\$", log10($n));
        }
        else {
            return $n;
        }
    }
    
    sub format_overhead {
        my $n = shift;
        my $d = shift;
        return "--" if ! $d;
        return (sprintf("%.1f", $n / $d));
    }
    
    sub format_speedup {
        my $n = shift;
        my $d = shift;
        return "--" if ! $d;
        return Testutils::format_sci($n / $d);
    }

    sub format_space {
        my $sp = shift;
        return "--" if ! $sp;
        return Testutils::format_size_full($sp, 2, "%.1f");
    }

    sub format_time_sec {
        my $t = shift;
        return "--" if ! $t;        
        if ($t >= 0.001) {
            return sprintf("%.2f", $t);
        }
        else {
            return Testutils::format_sci($t);
        }
    }
    
    sub format_time_sci {
        my $t = shift;        
        return "--" if ! $t;
        return Testutils::format_sci($t);
    }

    my $vars = {
        isverf => sub { my $x=shift; return 1 if $x eq "verf"; return 0; },
        apps   => $dataset->{apps},
        bins   => $dataset->{bins},
        format_input_size => \&format_input_size,
        format_overhead   => \&format_overhead,
        format_speedup    => \&format_speedup,
        format_space      => \&format_space,
        format_time_sec   => \&format_time_sec,
        format_time_sci   => \&format_time_sci,
    };
    
    ## The latex file from the template
    my $latex_file = $template;
    $latex_file =~ s/\.template$//;
   
    ## Create the directory for the latex, if it doesn't exist.
    Testutils::mysystem("mkdir -p $latex_dir");
    
    ## Process the latex template into latex
    $tt->process($template, $vars, "$latex_dir/$latex_file")
        || do {
            my $error = $tt->error();
            print "error type: ", $error->type(), "\n";
            print "error info: ", $error->info(), "\n";
            die;
    };
}

latex_from_template("tab-results-summary.tex.template");
