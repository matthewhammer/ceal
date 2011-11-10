#!/usr/bin/env perl
# Matthew Hammer <hammer@tti-c.org>
use strict;
use Getopt::Std;
use Testutils;

my $targetdir; # <- Command-line arg

sub doopts {
    my %args;
    getopts("d:", \%args);
    die "required arg: -d <target-dir>\n" if (!exists $args{d});
    $targetdir = $args{d};
}

doopts();

my @apps = Testutils::apps_in_path($targetdir);

my $columns = [["input-size", "size_10"],
               ["verifier/time", "frac"],
               ["fromscratch/time", "frac"],
               ["fromscratch/overhead", "frac"],
               ["allinsrem/time-ave", "sci"],
               ["allinsrem/speedup", "sci"],
#              ["allinsrem/all-malloc-bytes", "size_2"],
               ["allinsrem/all-maxlive-bytes", "size_2"]
               ];
my %cnts;
my %sums;

sub get_row {
    my $app = shift;
    my $input_size = shift;
    my $row = [];
        
    my $row_num = Testutils::find_row_num($targetdir.'/'.$app, "input-size", $input_size);
    
    if($row_num < 0) {
        print STDERR "Warning: missing data?: app=$app input_size=$input_size\n";
        return 0;
    }

    for my $column (@$columns) {
        my ($colname, $colformat) = @$column;
        my $data = Testutils::get_data($targetdir.'/'.$app, $colname, $row_num);
        push (@$row, $data);
    }
    return @$row;
}

sub format_row {
    my $row = shift;
    my $out = [];
    
    for(my $i = 0; $i < (scalar @$row); $i++) {
        my $data = @{$row}[$i];
        my $column = @{$columns}[$i];
        my ($colname, $colformat) = @$column;

        my $formatted = Testutils::format_number($data, $colformat);
        push (@$out, $formatted);        
    }    

    return @$out;
}

sub sum_row {
    my $input_size = shift;
    my $row = shift;
    
    $cnts{$input_size}++;

    if(exists $sums{$input_size}) {
        my @sum_row = @{$sums{$input_size}};
        for(my $i = 0; $i < (scalar @sum_row); $i++) {
            $sum_row[$i] += @{$row}[$i];
        }        
        $sums{$input_size} = [@sum_row];
    }
    else {
        $sums{$input_size} = $row;
    }
}

sub ave_row {
    my $input_size = shift;
    my @sum_row = @{$sums{$input_size}};
    my @ave_row = map {$_ / $cnts{$input_size}} @sum_row;
    return @ave_row;
}


open (OUT, ">$targetdir/summary-table.tex");

my $requests = Testutils::get_requests_file($targetdir,
                                            "summarize-data.local",
                                            "summarize-data.defaults");

for my $line (<$requests>) {
    next if $line =~ /^\#/;
    my ($app_name, $input_size) = Testutils::get_request_from_line $line;
    my @row = get_row($app_name, $input_size);
    
    if(@row) {
        sum_row($input_size, [@row]);
        my @formatted_row = format_row [@row];
        my $line = sprintf ("%20s & %s\\\\ \\hline\n", "{\\tt $app_name}", 
                            (join " & ", @formatted_row));
        Testutils::logg("Out", $line);
        printf OUT $line;
    }
}

for my $input_size (keys %sums) {
    print ("%% for input_size ", 
           Testutils::format_number($input_size, "size_10"),
           ", averages are: ",
           (join " & ", (format_row [ave_row $input_size])),
           "\n"
           );
}

close (OUT);
