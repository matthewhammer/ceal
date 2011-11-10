# Matthew Hammer <hammer@tti-c.org>
package CollectSeries;

use strict;
use Getopt::Std;
use Testutils;
use CollectSample;
use SeriesGen;

sub collect_series {
    my $targetdir = shift;
    my $command = shift;
    my $min_n = shift;
    my $max_n = shift;    
    my $num_samples = shift;
    
    my $n = $min_n;
    
    for($n  = $min_n; 
        $n <= $max_n;
        $n  = SeriesGen::next_n($n, $max_n)) 
    {        
        my $command_expanded = $command;

        # Substitute $n for \$N
        $command_expanded =~ s/\$N\W/$n/g;
        $command_expanded =~ s/\$N$/$n/g;
        
        Testutils::logg("Info", "running: `$command_expanded'");
        CollectSample::collect_sample($targetdir, $command_expanded, $num_samples);
    }
}

1;
