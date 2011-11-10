# Matthew Hammer <hammer@tti-c.org>
package Dataset;

use strict;
use POSIX;
use Testutils;
use Template;
use File::Basename;
use POSIX qw(log10);

sub load {
    my $targetdir = shift;
    my $requests  = shift;
    
    my $binaries     = {};
    my $applications = {};

    print "Finding the data ...\n";

    for my $line (<$requests>) {
        next if $line =~ /^\#/;
        my ($app_name, $max_n) = Testutils::get_request_from_line $line;
        my $find_path     = "$targetdir/$app_name";
        my @app_bin_paths = ();
        my $app_bins      = {};
        
        if ( -d $find_path ) {
            @app_bin_paths = split(/\n/,`find $find_path -mindepth 1 -maxdepth 1 -type d|sort`);
            
            print STDERR "Found: $find_path\n";
            
            for my $app_bin_path (@app_bin_paths) {            
                my $bin = basename($app_bin_path);            
                $app_bins->{$bin} = {
                    isverf => ($bin eq "verf"),
                    getd => sub {  
                        my $col_name = shift;
                        return Testutils::get_data($app_bin_path, $col_name, 0);
                    }
                };
            }
        }
        else {
            print STDERR "Warning: Couldn't find: `$find_path'\n";
            next;
        }
        
        sub nice_appname {
            my $name = shift;
            $name = ucfirst($name);
            #$name =~ s/_/\\\\_/g;
            return $name;
        }

        ## Application Info
        ##   We make this indexable in two ways:
        ##    1. From the name of the app
        ##    2. From each of the bins (binary types)
        my $app_info = { 
            name  => $app_name,
            label => nice_appname($app_name),
            path  => $find_path,
            bins  => $app_bins,
        };
        
        ## Map application name to application info
        $applications->{$app_name} = $app_info;
        
        ## For each cluster (app binary), add application info
        for my $app_bin (keys %$app_bins) {
            if(!(exists($binaries->{$app_bin}))){ 
                $binaries->{$app_bin} = [];
            }
            push @{$binaries->{$app_bin}}, $app_info;
        }
    }
    
    sub string_of_appinfo {
        my $x = shift;
        return ("{".
                "name => $x->{name}, ".
                "label => $x->{label}, ".
                "path => $x->{path}, ".
                "bins => [".join(" ", keys(%{$x->{bins}}))."] ".
                "}");
    }

    print "\nFound applications: \n";
    while ( my ($app, $info) = (each %$applications) ) {
        print ((string_of_appinfo $info), "\n");
    }
    print "\n";
    
    # Return the hashes we made.
    return { 
        apps => $applications, 
        bins => $binaries
    };
}

1;
