# Misc utilities for writing testing tools.
# Matthew Hammer <hammer@tti-c.org>
#
package Testutils;
use POSIX;
use strict;

sub logg {
    my $mode = shift;
    my $text = shift;
    chomp($text);    
    printf STDERR "== %4s ==    $text\n", $mode;
}

sub logg_sep {
    # (16 chars) 0 1 2 3 4 5 6 7 8
    my $sep   = "- - - - - - - - - ";
    logg("====", "$sep$sep$sep$sep$sep$sep$sep$sep");
}

sub mysystem {
    my $command = shift;
    logg("Sys", $command);
    system($command) == 0 || die $command;
}

sub min {
    my $a = shift; 
    my $b = shift; 
    if ($a < $b) { $a; } else {$b;}
}

sub apps_in_path {
    my $targetdir = shift;
    my $appdirs = [];

    opendir(DIR, $targetdir) or 
        die "can't opendir $targetdir: $!";

    while(defined(my $subdir = readdir(DIR))) {
        my $path = "$targetdir/$subdir";
        
        if(-d $path && !($subdir eq "." or $subdir eq ".." or $subdir =~ /^\..*/)) {
            push (@$appdirs, $subdir);
        }
    }    

    closedir(DIR);
    return @$appdirs;
}

sub entry_iscomment {
    my $entry = shift;
    return ($entry =~ /^\#.*/);
}

sub entry_stripped {
    my $entry = shift;
    $entry =~ s/(\w+)\s?/$1/g;
    return $entry;
}

sub find_row_num {
    my $path = shift;
    my $col_name = shift;
    my $entry = shift;
    my $row_num = 0;
    
    my $file = "$path/$col_name";
    open(FILE, $file) or die "can't open $file";
    while(<FILE>) {
        my $line = $_;
        
        next if(entry_iscomment $line);
        
        $line = entry_stripped($line);

        if ($line eq $entry) {
            return $row_num;
        }

        $row_num ++;
    }
    return -1;
}

sub get_data {
    my $path = shift;
    my $col_name = shift;
    my $row_num = shift;
    my $curr_row_num = 0;
    
    my $file = "$path/$col_name";
    open(FILE, "$file") or die "can't open $file";
    while(<FILE>) {
        my $line = $_;
        next if(entry_iscomment $line);
        if($curr_row_num eq $row_num) {
            $line = entry_stripped($line);
            return $line;
        }
        else {
            $curr_row_num ++;
            next;
        }
    }
    close(FILE);
}

sub resample_file {
    my $file_in = shift;
    my $file_out = shift;
    my $num_samples = int(shift);

    print "resample: $file_in, $file_out, $num_samples\n";

    open(FILE_IN, "<$file_in") or die "can't open: $file_in";
    open(FILE_OUT, ">$file_out") or die "can't open: $file_out";

    my $sum = 0;
    my $cnt = 0;
    my $min = "undef";
    my $max = "undef";

    while(<FILE_IN>) {
        my $line = $_;
        
        next if(Testutils::entry_iscomment $line);        

        chomp($line);
        
        $sum += $line;
        $max = $line if($line > $max || $max eq "undef");
        $min = $line if($line < $min || $min eq "undef");
        $cnt ++;

        if($cnt eq $num_samples) {
            if($num_samples > 2) {
                my $ave = ($sum - $min - $max) / ($num_samples - 2);
                print FILE_OUT "$ave\n";            
            }
            else {
                my $ave = $sum / $num_samples;
                print FILE_OUT "$ave\n";
            }
            $cnt = 0;
            $sum = 0;
            $min = "undef";
            $max = "undef";
        }
    }

    close(FILE_IN);
    close(FILE_OUT);
}

sub get_requests_file {
    my $targetdir     = shift;
    my $requests_file = shift;
    my $defaults_file = shift;
    my $file;

    if (! -d $targetdir) {
        die "target directory: `$targetdir' does not exist";
    }
    
    ## Is there a requests file in the target directory?
    if (! -f "$targetdir/$requests_file") {
        
        ## Is there a requests file in the current directory?
        if (! -f $requests_file) {

            ## Is there a defaults file in the current directory?
            die if ! -f $defaults_file;

            mysystem("cp $defaults_file $requests_file");
        }
        
        if($requests_file ne "$targetdir/$requests_file" ) {
            mysystem("cp $requests_file $targetdir/$requests_file");
        }
	else {
	    logg("Info", "$requests_file eq $targetdir/$requests_file");
	}
    }
    
    die if ! -f "$targetdir/$requests_file";
    open ($file, "< $targetdir/$requests_file");
    return $file;
}

sub get_number_from_abbrev {
    my $abbrev = shift;
    $abbrev =~ s/K$|k$/000/g;
    $abbrev =~ s/M$|m$/000000/g;
    $abbrev =~ s/G$|g$/000000000/g;
    return $abbrev;
}

sub get_request_from_line {
    my $line = shift;
    my ($app_name, $input_size_abbrev) = split /\s+/, $line;
    my $input_size = Testutils::get_number_from_abbrev $input_size_abbrev;
    return ($app_name, $input_size);
}

sub format_size_full {
    my $size = shift;
    my $base = shift;
    my $coeff_fmt = shift;
    
    my ($_1K, $_1M, $_1G);

    if($base eq 2) {
        $_1K = 1 << 10;
        $_1M = 1 << 20;
        $_1G = 1 << 30;
    }
    elsif($base eq 10) {
        $_1K = 1000;
        $_1M = 1000000;
        $_1G = 1000000000;
    }
    else {
        die;
    }
    
    my $suffix = "";
    my $coeff;

    if($size < $_1K) {
        $coeff = $size;
        $suffix = "";
    }
    elsif($size < $_1M) {
        $coeff = $size / $_1K;
        $suffix = "K";
    }
    else {
        $coeff = $size / $_1M;
        $suffix = "M";        
    }
#    elsif($size < $_1G) {
#        $coeff = $size / $_1M;
#        $suffix = "M";
#    }
#    else {
#        $coeff = $size / $_1G;
#        $suffix = "G";
#    }
    return sprintf("$coeff_fmt%s", $coeff, $suffix);
}

sub format_size {
    my $size = shift;
    my $base = shift;    
    return format_size_full($size, $base, "%.1f");
}

sub format_sci {
    my $number = shift;
    my $s = sprintf("%.1e", $number);
    $s =~ s/e\-0+(\d+)/ \\times 10^\{-$1\}/g;
    $s =~ s/e\+0+(\d+)/ \\times 10^\{$1\}/g;
    return "\$$s\$";
}

sub format_number {
    my $number = shift;
    my $format = shift;
    
    if($format eq "size_2") {
        return format_size($number, 2);
    }
    elsif($format eq "size_10") {
        return format_size($number, 10);
    }
    elsif($format eq "frac") {
        return sprintf("%.1f", $number);
    }
    elsif($format eq "sci") {
        return format_sci($number);
    }
    else {
        die "format not recognized: $format";
    }
}

1;
