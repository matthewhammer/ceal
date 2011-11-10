#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
#
# Generates a little report fragment describing the machine it runs
# on.  Use -t to get output ready for a TeX table.

use strict;
use Getopt::Std;

my %args;
getopts("t", \%args);

sub max {
    my $a = shift;
    my $b = shift;
    my $max;    
    if($a > $b) { $max = $a; } 
    else { $max = $b; }
    $max;
}

sub info_from_command {
    my $info = shift;
    my $command = shift;
    my $result = `$command`;
    chomp($result);
    $$info{$command} = $result;
}

sub info_from_commands {
    my $info = shift;
    my @commands = ("uname -n",
                    "uname -s",
                    "uname -r",
                    "uname -m",
                    "uname -p",
                    "gcc -dumpversion");    
    for my $command (@commands) {
        info_from_command($info, $command);
    }
}

sub info_from_svninfo {
    my $svnbase = "../..";
    my $svndir = "$svnbase/.svn";

    if( -d $svndir  ) {
        my $info = shift;
        my @lines = split /\n/, `svn -R info ../..`;
        
        for my $line (@lines) {        
            if($line =~ /URL:(.+)/) {
                $$info{"svnurl"} = $1 
                    if !exists($$info{"svnurl"});
            }
            elsif($line =~ /Revision:(.+)/) {
                $$info{"svnrev"} = max($1, $$info{"svnrev"});
            }
        }
    }
    else {
        die ("can't find .svn here: `$svnbase'\n".
             "Try running with workding dir eval/scripts/\n");
    }
}

sub print_info {
    my $info = shift;
    my @sorted_keys = sort(keys %$info);
    for my $key (@sorted_keys) {
        if(exists($args{t})) {
            my $val = $$info{$key};
            $val =~ s/^\s+//g;
            $val =~ s/\s+$//g;
            printf "\{\\tt %20s\} & \{%s\}\\\\\n", $key, $val;
        }
        else {
            print "$key = $$info{$key}\n";
        }
    }
}

my $info = {};

info_from_commands($info);
info_from_svninfo($info);
print_info($info);

