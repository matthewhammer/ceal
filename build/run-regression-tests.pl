#!/usr/bin/env perl
# matthew hammer <hammer@tti-c.org>

use strict;

my @test_options=@ARGV; 
my $bin_path='../bin';
my $CEAL_LOG='ceal.log';
my $fail_out = "regression-failures.out";
my $badstacktest = "./test-stack-magic-numbers.pl";

#  -- How do we find the regression tests to run? --
#
#  We look for scripts under $bin_path whose names end with
#  'regression-test.sh'. We run each one of these scripts in turn and
#  check its exit status.  (See compile-app.sh to see how these
#  scripts are generated to invoke each binary that we compile.)
#
my $find_command = ("find $bin_path ".
                    "-type f ".
                    "-name '*.regression-test.sh'");

my @commands_to_run = split(/\n/, `$find_command | sort`);

my @commands_that_succeeded = ();
my @commands_that_failed = ();
my @output_from_failures = ();

print "Enqueued tests: \n";
print ((join "\n", @commands_to_run));
print ".\n\n";

for my $command (@commands_to_run) {    
    my $out = $command;
    $out =~ s/\.sh/.out/;    
    
    my $piped_command = "$command @test_options &> $out";
    
    system("rm -rf $CEAL_LOG");

    print "Running: $command ...";
    
    if( system($piped_command) ) {
        push @commands_that_failed, [$command, $?];
        push @output_from_failures, $out;

        my $has_bad_stack = 0;

        if( ( -f $CEAL_LOG ) && 
            ( -f $badstacktest ) ) 
        {
            $has_bad_stack = 1 if
                system("$badstacktest &> $out.stack-test");
        }

        if ($has_bad_stack) {
            print "\x1B[1;33m";
            print "BADSTACK";
            print "\x1B[0m";
            print ".\n";
        }
        else {
            print "\x1B[1;31m";
            print "FAIL";
            print "\x1B[0m";
            print ".\n";
        }
    }
    else {
        push @commands_that_succeeded, $command;
        print "\x1B[1;32m";
        print "Okay";
        print "\x1B[0m";
        print ".\n";
    }
}
print "All tests finished.\n\n";

## Say what succeeded
printf "Succeeded (%d/%d):\n", (scalar @commands_that_succeeded), (scalar @commands_to_run);
print ((join "\n", @commands_that_succeeded));
print ".\n\n";

## Say what failed
printf "Failed (%d/%d):\n", (scalar @commands_that_failed), (scalar @commands_to_run);
for my $command_status  (@commands_that_failed) {
    my ($command, $status) = @$command_status;
    printf "$status $command\n";
}
print ".\n\n";


## Create/overwrite $fail_out
## This file summarizes all the failure output.
if( scalar (@output_from_failures) ) {

    system("date > $fail_out; echo >> $fail_out");    
    system("echo The following commands failed their regression tests: >> $fail_out");
    
    for my $command_status ( @commands_that_failed ) {
        my ($command,$status) = @$command_status;
        system("echo '  ' $command >> $fail_out");
    }
    
    system("echo >> $fail_out");

    my @commands = @commands_that_failed;
    for my $output (@output_from_failures) {
        my $command_status = shift @commands;
        my ($command,$status) = @$command_status;
        my $sep = "- - - - - - - - - - " ;
        system("echo $sep $sep $sep $sep >> $fail_out");
        system("echo $command >> $fail_out");
        system("echo $output >> $fail_out");
        system("echo >> $fail_out");
        system("cat $output >> $fail_out");
    }
    print "Output from failures written to: $fail_out\n";
}
