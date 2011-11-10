# Matthew Hammer <hammer@tti-c.org>
# Taken directly from code by Umut Acar et al.

package SeriesGen;
use POSIX;
use strict;
use Testutils;

my $factor = 1.35;

## This generates the next number in a series of input sizes.
sub next_n {
  my $n     = shift; 
  my $max_n = shift;
  my $new_n = 0; 

  if ($n < 10) {
      $new_n = $n+1;
  }
  elsif ($n eq $max_n) {
    $new_n = $n+1; 
  }
  else {
    $new_n = Testutils::min($max_n, floor($n * $factor));
  };
  
  if ($n < 64000 && $new_n >= 64000) {
      $new_n = 64000;
  }
  else { };

  if ($n < 100000 && $new_n >= 100000) {
      $new_n = 100000;
  }
  else { };

  if ($n < 150000 && $new_n >= 150000) {
      $new_n = 150000;
  }
  else { };

  if ($n < 500000 && $new_n >= 500000) {
      $new_n = 500000;
  }
  else { };

  if ($n < 1000000 && $new_n >= 1000000) {
      $new_n = 1000000;
  }
  else { };

  if ($n < 1500000 && $new_n >= 1500000) {
      $new_n = 1500000;
  }
  else { };

  if ($n < 2000000 && $new_n >= 2000000) {
      $new_n = 2000000;
  }
  else { };
  
  $new_n;    
}

1;
