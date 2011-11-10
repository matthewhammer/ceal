#!/usr/bin/env perl 
# matthew hammer <hammer@tti-c.org>
use strict;

my $app_path='../src/apps';

## Applications each have a directory under $app_path
## We try to automatically find them here.
## We ignore the 'common' and SVN directories.
my $find_command = ("find $app_path ".
                    "-maxdepth 1 -mindepth 1 ".
                    "-type d ".
                    "-not -name 'common' ".
                    "-not -name 'functors' ".
                    "-not -name 'arrtree_sum' ".
                    "-not -name 'kmeans' ".
                    "-not -name 'mapreduce_wordcount' ".
                    "-not -name '*rope*' ".
                    "-not -name '\.*' ");

#                    "-not -name '*sparsemat*' ".

my @app_dirs=split(/\n/, `$find_command | sort`);

my @commands_to_run = ();

my @commands_that_failed = ();
my @commands_that_succeeded = ();

for my $app_dir (sort (@app_dirs)) {
    my $app_name = `basename $app_dir`;
    chomp $app_name;
    my $command = "./compile-app.sh $app_name";
    push @commands_to_run, $command;
    print "enqueued command: $command\n";
}

for my $command (@commands_to_run) {
    if(system($command)) {
        push @commands_that_failed, $command;
    }
    else {
        push @commands_that_succeeded, $command;
    }
}

print "Succeeded: \n";
print ((join "\n", @commands_that_succeeded));
print ".\n\n";

print "Failed: \n";
print (join("\n", @commands_that_failed));
print ".\n\n";
