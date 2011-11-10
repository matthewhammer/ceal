#!/usr/bin/env perl
use strict;
use Getopt::Std;

my $srcdir = $ARGV[0];
my $tgtdir = $ARGV[1];

die "source directory `$srcdir' does not exist" if(! -d $srcdir);
die "target directory `$tgtdir' does not exist" if(! -d $tgtdir);

print ("Copying matching PDFs from ",
       "source tree to the target tree:\n");

print "Source: $srcdir\n";
print "Target: $tgtdir\n";

my @files = split(/\n/, `find $srcdir -name '*.pdf'`);

sub print_file {
    my $file    = shift;
    my $matches = shift;
    my $prefix = "... ";

    if($matches) {
        $prefix = "COPY";
    }
    
    print $prefix, " ", $file, "\n";
}

for my $file (@files) {
    my $matches = 0;

    if($file =~ /\/steps-.*.pdf/) {
        $matches = 1;
    }
    
    print_file($file, $matches);

    if($matches) {
        if($file =~ /^$srcdir(\/?)(.+)/) {
            my $subtree = `dirname $2`;
            chomp($subtree);
            my $tgtsubtree = "$tgtdir/$subtree";
            system("mkdir -p $tgtsubtree");
            die if ! -d $tgtsubtree;
            system("cp $file $tgtsubtree");
            print ">>>> $tgtsubtree\n";
        }
        else {
            print $srcdir, "\n";
            print $file, "\n";
            die;
        }
    }
}
