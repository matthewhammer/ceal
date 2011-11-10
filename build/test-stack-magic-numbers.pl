#!/usr/bin/env perl
#
# This is a diagostic tool that I'm using to determine if and how the
# stack discipline employed by my runtime system may be
# malfunctioning.  The input to this program is a file called
# "ceal.log".  See CEAL_LOGGING in the libceal Makefile.

use strict;

die "Couldn't find: ceal.log\n" if ( ! -f 'ceal.log' );

my @raw_lines = split /\n/, `cat ceal.log | grep "magic number"`;

my @magic_numbers;

for my $raw (@raw_lines) {
    my $magic_number = $raw;
    
    $magic_number =~ s/.+magic number (\w+)\W*.*/\1/ 
        || die "cannot understand this line: $raw";

    if( $raw =~ /pushed/ ) {
        #print "pushed: $magic_number\n" ;
        push @magic_numbers, $magic_number;
    }
    elsif( $raw =~ /popped/ ) {
        my $local_magic_number = pop @magic_numbers;
        #print "popped: $magic_number $local_magic_number\n";
        die "$local_magic_number (what i wanted) <> $magic_number (what i found)."
            unless $local_magic_number eq $magic_number;
    }
    else {
        die "cannot understand this line: $raw";
    }   
}

if ( scalar ( @magic_numbers ) ) {
    print (join("\n", @magic_numbers));
    die "magic numbers never popped\n";
}

print "Everything appears correct.\n"
