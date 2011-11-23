/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdio.h>
#include <stdlib.h>
#include "cealtesthook_dummies.c"

#define print(FMT,...) printf("%s:%d: " FMT "\n", __FUNCTION__,__LINE__,##__VA_ARGS__)

// #define BASIC_VERSION
//#define SEPARATE_FUN_VERSION
// #define CUT_BLOCK_VERSION
 #define CUT_EXPR_VERSION
// #define CUT_EXPR_VERSION_2
// #define MEMO_STMT_VERSION


#ifdef BASIC_VERSION
/* This version is the most straightforward. */
/* But, if we pretend that squaring a long is very costly, then it is
   not stable: sometimes it will re-evaluate this operation
   needlessly. */
void max_of_squares(long* in1, long* in2, long* out) {
  long max;
  
  if(labs(*in1) > labs(*in2)) {
    max = labs(*in1);
  }
  else {
    max = labs(*in2);
  }

  print("max is %ld, computing its square..", max);
  *out = (max * max);
}

#elif defined SEPARATE_FUN_VERSION
long max_of_labs(long* in1, long* in2) {
  return labs(*in1) > labs(*in2)
    ? labs(*in1)
    : labs(*in2) ;
}

void max_of_squares(long* in1, long* in2, long* out) {
  long max = max_of_labs(in1, in2);
  print("max is %ld, computing its square..", max);
  *out = (max * max);
}

#elif defined CUT_BLOCK_VERSION
void max_of_squares(long* in1, long* in2, long* out) {
  long max;

  cut {
    if(labs(*in1) > labs(*in2)) {
      max = labs(*in1);
    }
    else {
      max = labs(*in2);
    }
  }

  print("max is %ld, computing its square..", max);
  *out = (max * max);
}

#elif defined CUT_EXPR_VERSION

/* Mustafa Zengin pointed out the following subtly to me: on nearly
   all input changes, this code avoids the needless reevaluation of
   squaring `max`.  However, even so, there are still many cases where
   it doesn't: whenever the max of the two inputs changes sign, from
   positive to negative (or vice versa), the second pointer
   dereference of that input value is affected.  Suppose, for
   instance, that the input values are `1` and `2`, and then the
   second value is changed from `2` to `-2`.  Then read in the
   statement `max = labs(*in2)` is affected and will be
   reevaluated.  */

void max_of_squares(long* in1, long* in2, long* out) {
  long max;

  /* The cast to long is needed by CEAL for obscure reasons.
     (See also: the 'minimum-modref-size' restriction). */
  if(cut( (long) (labs(*in1) > labs(*in2)) )) {
    max = labs(*in1);
  }
  else {
    max = labs(*in2);
  }

  print("max is %ld, computing its square..", max);
  *out = (max * max);
}

#elif defined CUT_EXPR_VERSION_2

void max_of_squares(long* in1, long* in2, long* out) {
  long max;

  /* The cast to long is needed by CEAL for obscure reasons.
     (See also: the 'minimum-modref-size' restriction). */
  if(cut( (long) (labs(*in1) > labs(*in2)) )) {
    max = cut(labs(*in1));
  }
  else {
    max = cut(labs(*in2));
  }

  print("max is %ld, computing its square..", max);
  *out = (max * max);
}

#elif defined MEMO_VERSION 
void max_of_squares(long* in1, long* in2, long* out) {
  long max;
  
  if(labs(*in1) > labs(*in2)) {
    max = labs(*in1);
  }
  else {
    max = labs(*in2);
  }

  memo;
  print("max is %ld, computing its square..", max);
  *out = (max * max);
}

#endif

void print_inout(long* in1, long* in2, long* out) {
  print("input values are %ld and %ld", *in1, *in2);
  print("max of squares is %ld", *out);
}


int main(int argc, char** argv) {

  /* Construct input. */
  long x = 2;
  long y = -3;

  /* Space for output. */
  long z;

  /* Invoke the core. */
  print("running core..");
  core(max_of_squares)(&x, &y, &z);
  print_inout(&x, &y, &z);
  
  /* Mutate the input. */     
  x = -4;

  /* Invoke change propagation. */
  print("running change propagation..");
  propagate;
  print_inout(&x, &y, &z);

  /* Mutate the input. */     
  y = 3;

  /* Invoke change propagation. */
  print("running change propagation..");
  propagate;
  print_inout(&x, &y, &z);

  /* Mutate the input. */     
  x = 4;

  /* Invoke change propagation. */
  print("running change propagation..");
  propagate;
  print_inout(&x, &y, &z);

  return 0;
}
