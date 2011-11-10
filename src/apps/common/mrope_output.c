/* Mike Rainey <mrainey@mpi-sws.org>   */
/* Matthew Hammer <hammer@mpi-sws.org> */

#ifndef __MROPE_OUTPUT_C__
#define __MROPE_OUTPUT_C__

#include <stdio.h>
#include <assert.h>

mrope_t mrope_output_core;
mrope_t mrope_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  if(i == 0)
    mrope_fprint(file, mrope_output_core);
  else if(i == 1)
    mrope_fprint(file, mrope_output_verf);
  else
    abort();
}

static int check_mropes_rec(mrope_t rp1, mrope_t rp2) {
  if( mrope_node(rp1) != mrope_node(rp2) ) {
    fprintf(stderr, __FUNCTION__ ": ropes are not the same shape:"
	    " %p %p\n", rp1, rp2);
    return 0;
  }
  else {
#if 0
    fprintf(stderr, __FUNCTION__ ": OK BEGIN : %p %p\n", rp1, rp2);
#endif

    switch( mrope_node(rp1) ) {
    case CAT:
      return
        check_mropes_rec(mrope_left(rp1), mrope_left(rp2)) &&
        check_mropes_rec(mrope_right(rp1), mrope_right(rp2));
    case LEAF:
      return DATA_EQUAL(mrope_data(rp1), mrope_data(rp2));
    case EMPTY:
      return 1;
    }
  }

#if 0
  fprintf(stderr, __FUNCTION__ ": OK END : %p %p\n", rp1, rp2);
#endif

  abort();
  return 0;
}

int cealtesthook_output_check() {  
  return check_mropes_rec(mrope_output_core,
			  mrope_output_verf);
}

#endif
