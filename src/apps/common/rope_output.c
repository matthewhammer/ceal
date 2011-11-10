/* Mike Rainey <mrainey@mpi-sws.org>   */
/* Matthew Hammer <hammer@mpi-sws.org> */

#ifndef __ROPE_OUTPUT_C__
#define __ROPE_OUTPUT_C__

#include <stdio.h>
#include <assert.h>

rope_m_t rope_output_core;
rope_m_t rope_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  if(i == 0)
    rope_fprint(file, rope_output_core);
  else if(i == 1)
    rope_fprint(file, rope_output_verf);
  else
    abort();
}

static int check_ropes_rec(rope_m_t rp1, rope_m_t rp2) {
  if( rope_type(rp1) != rope_type(rp2) )
    return 0;
  else {
    switch( rope_type(rp1) ) {
    case CAT:
      return
        check_ropes_rec(rope_left(rp1), rope_left(rp2)) &&
        check_ropes_rec(rope_right(rp1), rope_right(rp2));
    case LEAF:
      return DATA_EQUAL(rope_elt(rp1), rope_elt(rp2));
    case EMPTY:
      return 1;
    }
  }
  abort();
  return 0;
}

int cealtesthook_output_check() {  
  return check_ropes_rec(rope_output_core,
                         rope_output_verf);
}

#endif
