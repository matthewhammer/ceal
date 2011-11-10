/* Matthew Hammer <hammer@tti-c.edu> */
/*
  This example demonstrates how to use the ceal_pool allocator.
*/

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"
#include "scalar_input.c"
#include "scalar_output.c"
#include "main.c"

#include "pool.c"

typedef struct {
  int x;
  int y;
} point_t;

static void test_ffi(long owcr * in, long owcr * out) {
  if ( *in % 2 ) {    
    /* As the input changes from even to odd, the allocations below
       will be invoked; in the other direction, they are revoked. */
    
    ceal_pool_t* pool = ceal_pool_begin();
    point_t* p1 = (point_t*) ceal_pool_malloc( pool, sizeof(point_t) );
    point_t* p2 = (point_t*) ceal_pool_malloc( pool, sizeof(point_t) );
    point_t* p3 = (point_t*) ceal_pool_malloc( pool, sizeof(point_t) );
    /*
     * Compute something with points p1, p2 and p3
     *    .
     *    .
     *    .
     * Done computing.  Don't need points anymore.
     *
     */    
    ceal_pool_end( pool );
  }
  *out = 0;
}

void cealtesthook_run_core() {
  core(test_ffi)(&scalar_input, &scalar_output_core);
}

void cealtesthook_run_verf() {
  test_ffi(&scalar_input, &scalar_output_verf);
}
