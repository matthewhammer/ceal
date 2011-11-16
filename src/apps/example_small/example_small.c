#include <stdio.h>
#include <stdlib.h>
#include "cealtesthook_dummies.c"


/* This version is the most straightforward. */
void max_of_squares_1(long* in1, long* in2, long* out) {
  long max;
  
  if(labs(*in1) > labs(*in2)) {
    max = *in1;
  }
  else {
    max = *in2;
  }

  printf(__FUNCTION__  ": max is %ld, computing its square..\n", max);
  *out = (max * max);
}

/* This version is like max_squares_1, except that it avoids unneeded
   re-multiplication via a memoization point. */
void max_of_squares_2(long* in1, long* in2, long* out) {
  long max;
  
  if(labs(*in1) > labs(*in2)) {
    max = *in1;
  }
  else {
    max = *in2;
  }

  memo;
  printf(__FUNCTION__ ": max is %ld, computing its square..\n", max);
  *out = (max * max);
}

/* This version is like max_squares_1, except that it avoids unneeded
   re-multiplication via a cut block. */
void max_of_squares_3(long* in1, long* in2, long* out) {
  long max;

  cut {
    if(labs(*in1) > labs(*in2)) {
      max = *in1;
    }
    else {
      max = *in2;
    }
  }

  printf(__FUNCTION__ ": max is %ld, computing its square..\n", max);
  *out = (max * max);
}

void print_inout(long* in1, long* in2, long* out) {
  printf(__FUNCTION__ ": input values   : %ld and %ld\n", *in1, *in2);
  printf(__FUNCTION__ ": max of squares : %ld\n", *out);
}


int main(int argc, char** argv) {

  /* Construct input. */
  long x = 2;
  long y = -3;

  /* Space for output. */
  long z;

  /* Invoke the core. */
  core(max_of_squares_1)(&x, &y, &z);
  print_inout(&x, &y, &z);
  
  /* Mutate the input. */     
  x = -4;

  /* Invoke change propagation. */
  propagate;
  print_inout(&x, &y, &z);

  /* Mutate the input. */     
  y = 3;

  /* Invoke change propagation. */
  propagate;
  print_inout(&x, &y, &z);

  return 0;
}
