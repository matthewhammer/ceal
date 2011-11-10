# Matthew Hammer <hammer@tti-c.org>

# This tool (the subroutine collect_sample) runs a given shell command
# (an argument) and collects its stdout and stderr, which it processes
# as follows:

# 1. It looks for lines matching:
#
#    ">> filename : value [(comment)] .*" 
#
# Where whitespace is treated as delimiters; ">>" and ":" and "(", ")"
# are literal strings; "filename", "comment" and "value" are
# continuous sequences of word characters; and "[", "]" aren't literal
# but only indicate that the comment is optional.

# 2. It appends reformatted versions of these lines to the given
# filename, prefixed by the target directory (also given as an
# argument).  The filename can contain relative paths and
# subdirectories.  Two lines are written for every original line.  The
# first begins with a "#" and contains any comment given.  The second
# contains the value.

# 3. Each line of output, whether it matches the above pattern or not,
# is written to stderr, prefixed with "== Out == ", where whitespace
# may vary.  Other lines of output are also written to stderr (with
# different prefixes) and contain other debugging information.

# == TODO ==
#
# ++ Currently there is no detection of the exit code of the command.
# This is a huge flaw.  It's mainly because I don't currently know how
# to run a shell command and capture its stdout, stderr and exit code
# all together.


package CollectSample;
use File::Basename;
use strict;
use Testutils;

## Global state:
my %filehandles;

## Global state:
my @random_seeds = ();


sub get_ith_random_seed {
    my $index = shift;
    while ( ! exists $random_seeds[ $index ] ) {
        my $RAND_MAX = 2147483647;
        my $seed     = int(rand($RAND_MAX));
        my $nextidx  = scalar(@random_seeds);
        push @random_seeds, $seed;
        Testutils::logg("Info", "fixing random seed $nextidx as: $seed");
    }    
    return $random_seeds[ $index ];
}

sub close_filehandles {
    Testutils::logg("Info", "Closing all the open file handles");
    while(my ($path, $handle) = each(%filehandles)) {
        Testutils::logg("Info", "Closing file handle for: $path");
        close($handle);
    }
    Testutils::logg("Info", "Ok. Closed all open file handles");
    # BUG-FIX: Need to clear this as follows:
    %filehandles = ();
}

sub get_filehandle {
    my $path = shift;
    
    #Testutils::logg("Info", "path:`$path'");

    my $dirname = dirname($path);
    
    #Testutils::logg("Info", "dirname:`$dirname'");

    chomp($dirname);

    #Testutils::logg("Info", "chomp(dirname): `$dirname'");
    
    if( ! (-d $dirname) ) {
        Testutils::mysystem("mkdir -p $dirname");
    }

    if (exists $filehandles{$path}) {
        return $filehandles{$path};
    }
    else {
        open (my $filehandle, ">> $path") 
            || do {
                Testutils::logg("Info", "Couldn't open for appending (1st try): $path");
                Testutils::logg("Info", "Going to close all open file handles...");
                close_filehandles();
                open (my $filehandle, ">> $path")
                    or die "Couldn't open for appending (2nd try): $path";
        };
        
        $filehandles{$path} = $filehandle;

        return $filehandle;
    }
}

sub append_entry {
    my $file = shift;
    my $sample_num = shift;
    my $text = shift;
    my $comment = shift;

    chomp($file); $file = "$file";
    chomp($text);
    chomp($comment);

    my $logstring = sprintf(">> %-64s : %32s %32s", $file, $text, "($comment)");

    #Testutils::logg("Info", "file:`$file'");

    Testutils::logg("Data", $logstring) if $comment;

    my $fh = get_filehandle $file;
    
    print $fh "## sample: #$sample_num\n";
    print $fh "## $comment\n" if $comment;
    print $fh "$text\n";
}

sub append_date_now {
    my $file = shift;    
    my $sample_num = shift;
    my $text = `date +'%s.%N'`;
    my $comment = `date`;    
    append_entry($file, $sample_num, $text, $comment);
}

sub collect_sample {
    my $targetdir = shift;
    my $command = shift;
    my $num_samples = shift;
    
    for(my $sample_num = 1; 
        $sample_num <= $num_samples; 
        $sample_num++) 
    {
        # CHOOSING RANDOM SEEDS:
        # Every ith sample uses the same random seed.
        # If we compare different versions of a single benchmark, this comes in handy.
        # So, let i=sample_num; substitute ith random number for \$R in command.
        my $command_expanded = "$command";
        my $random_seed      = get_ith_random_seed($sample_num);
        $command_expanded =~ s/\$R(\W)/$random_seed$1/g;
        $command_expanded =~ s/\$R$/$random_seed/g;

        Testutils::logg_sep();
        Testutils::logg("Info", "collect_sample(\"$targetdir\", \"$command_expanded\") ".
                        "(Sample #$sample_num of $num_samples)");

        append_date_now("$targetdir/start-date.$num_samples",$sample_num);

        Testutils::logg("Info", "running: `$command_expanded'");
        open FH, "$command_expanded 2>&1|" or die "failed to run: `$command_expanded'";
    
        while(<FH>) {    
            if( />>\s*(\S+)\s*:\s*(\S+)(\s+\((.+)\))?/ ) {
                append_entry("$targetdir/$1.$num_samples",$sample_num,$2,$4);
            }
            else {
                Testutils::logg("Out", $_); 
              }
        }
        append_date_now("$targetdir/stop-date.$num_samples",$sample_num);
          
    }
}

1;
