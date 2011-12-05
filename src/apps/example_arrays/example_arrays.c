/* Matthew Hammer <hammer@mpi-sws.org> */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cealtesthook_dummies.c"

#define db(FMT,...) printf("-- %s:%d: " FMT "\n", __FUNCTION__,__LINE__,##__VA_ARGS__)

/* Let's have arrays that hold longs. */
typedef long data_t;

data_t* alloc_array(long num) {
  data_t* A = (data_t*) malloc(sizeof(data_t) * num);
  memset((void*)A, 0, sizeof(data_t) * num);
  return A;
}

/* A Semi-group operation: Some operation that is associative
   (over elements for which the operation is closed). */
data_t semigroup_op(data_t a, data_t b) {
  db("");
  return a + b;
}

/* We exploit the fact that the reduction is associative. */
void array_reduce(data_t* in, long len, data_t* out)  {
  db("");
  while( len > 1 ) {
    db("");
    for( long i = 0; i < len - 1; i += 2 ) {
      db("");
      data_t reduced = semigroup_op( in[i], in[i+1] );
      db("");
      in[ i/2 ] = reduced;
    }
    len /= 2;
  }
  *out = *in;
}

int main(int argc, char** argv) {

#if 1
  data_t in[8];
  data_t* in2 = in;
#else
  data_t* in = alloc_array(8);
#endif
  
  in[0] =  0;
  in[1] = +1;
  in[2] = -2;
  in[3] = -3;
  in[4] = +4;
  in[5] = -5;
  in[6] = +6;
  in[7] = -7;

  data_t out;
  
  core(array_reduce)(in2, 8, &out);
  printf("out is %ld\n", out);
  
  in[3] = +3;
  propagate;
  printf("out is %ld\n", out);

  in[3] = -3;
  propagate;
  printf("out is %ld\n", out);
  
  return 0;
}
