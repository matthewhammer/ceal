#!/usr/bin/env perl

#
# A clumsy script to create a comparison table between SaSML and CEAL numbers.
#

use strict;
use Testutils;

#my $SASML_DATADIR = "/home/hammer/papers/cps/src/experiments-mlton/results/dynamic/apps-20081025-sinop";
#my $SASML_DATADIR = "/home/hammer/papers/slime-compiler/pldi2009/eval/results.sinop__2008-11-05.B/SaSML";
my $SASML_DATADIR = "/home/hammer/papers/slime-compiler/pldi2009/eval/apps-20081025-sinop";
#my $CEAL_DATADIR = "/home/hammer/papers/slime-compiler/pldi2009/eval/results.sinop__2009-03-18";
my $CEAL_DATADIR = "/home/hammer/papers/slime-compiler/pldi2009/eval/results.sinop__2009-03-21";

# Our CEAL benchmark names paired with the SaSML counterparts.
my $apps = [
            ["filter", "filter"],
            ["map", "map"],
            ["reverse", "reverse"],
            ["minimum", "minimum"],
            ["sum","sum"],
            ["quicksort", "qsort-string"],
#            ["mergesort", "msort-string"],
            ["quickhull", "quick-hull"],
            ["diameter", "diameter"],
            ];

# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## Get the SASML data:
sub get_sasml_data {
    my $app = shift;
    
    sub get_last {
        my $app = shift;
        my $sub_path = shift;
        return (split /\s+/, `cat $SASML_DATADIR/$app/$sub_path | tail -n 1 `);
    }
    
    sub get_max_live {
        my $app = shift;
        my $sub_path = shift;
        my $text = `cat $SASML_DATADIR/$app/$sub_path`;
        my @entries = split /ProfileAllInsertDelete/, $text;        
        my $last = pop @entries;
        
        die unless $last =~ /max bytes live: ([\d,]+)/;
        my $max_live = $1;
        $max_live =~ s/,//g;
        return $max_live;
    }

    my ($input_size_1, $self_time)   = get_last $app, "InitialRun/full.dat";
    my ($input_size_2, $update_time) = get_last $app, "AllInsertDelete/full.dat";
    my $maxlive                      = get_max_live $app, "AllInsertDelete/log.dat";
    
    die unless $input_size_1 == $input_size_2;
    
    return { 
        app => $app,
        input_size => $input_size_1,
        self_time => $self_time,
        update_time => $update_time,
        maxlive => $maxlive 
        };
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - 
sub get_ceal_data {
    my $app = shift;
    my $input_size = shift;

    my $row_num = Testutils::find_row_num($CEAL_DATADIR.'/'.$app, "input-size", $input_size);
    
    die "can't find ceal data for: $app\@$input_size" if $row_num == -1;
    
    sub get {
        my $app = shift;
        my $row_num = shift;
        my $colname = shift;
        
        my $data = Testutils::get_data($CEAL_DATADIR.'/'.$app, $colname, $row_num);

        return $data;
    }
    
    return {
        app => $app,
        input_size => $input_size,
        self_time => (get $app, $row_num, "fromscratch/time"),
        update_time => (get $app, $row_num, "allinsrem/time-ave"),
        maxlive => (get $app, $row_num, "allinsrem/all-maxlive-bytes"),
    }
}

# For computing averages:
my %ratio_sums;

print "%% CEAL_DATADIR=$CEAL_DATADIR \n";
print "%% SASML_DATADIR=$SASML_DATADIR \n";
for my $appnames (@$apps) {
    my ($name, $sasml_name) = @$appnames;
    
    my $sasml = get_sasml_data $sasml_name;     
    my $ceal  = get_ceal_data $name, $sasml->{input_size};
    
    my $self_ratio = $sasml->{self_time} / $ceal->{self_time};
    my $update_ratio = $sasml->{update_time} / $ceal->{update_time};
    my $maxlive_ratio = $sasml->{maxlive} / $ceal->{maxlive};
    
    $ratio_sums{self_time} += $self_ratio;
    $ratio_sums{update_time} += $update_ratio;
    $ratio_sums{maxlive} += $maxlive_ratio;

    printf
        "%20s & %s\\\\ \\hline\n",
        "{\\tt $name}",
        (join " & ",
         (Testutils::format_number($sasml->{input_size}, "size_10"),
          #
          #"---",
          #
          Testutils::format_number($ceal->{self_time}, "frac"),
          Testutils::format_number($sasml->{self_time}, "frac"),
          Testutils::format_number($self_ratio, "frac"),
          #
          #"---",
          #
          Testutils::format_number($ceal->{update_time}, "sci"),
          Testutils::format_number($sasml->{update_time}, "sci"),
          Testutils::format_number($update_ratio, "frac"),
          #
          #"---",
          #
          Testutils::format_number($ceal->{maxlive}, "size_2"),
          Testutils::format_number($sasml->{maxlive}, "size_2"),
          Testutils::format_number($maxlive_ratio, "frac")
          )
         );
}

printf "%% self_time:   average ratio: %.1f\n", ($ratio_sums{self_time} / (scalar @$apps));
printf "%% update_time: average ratio: %.1f\n", ($ratio_sums{update_time} / (scalar @$apps));
printf "%% maxlive:     average ratio: %.1f\n", ($ratio_sums{maxlive} / (scalar @$apps));
