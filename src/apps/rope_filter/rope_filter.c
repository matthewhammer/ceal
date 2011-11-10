/* Mike Rainey <mrainey@mpi-sws.org> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "rope.c"
#include "rope_input.c"
#include "rope_output.c"
#include "main.c"

/* rope_filter_1 (rp) */
/* O(log n) first pass in the rope filter algorithm */
/* result is the filtered rope, which may or may not be balanced */
rope_m_t
rope_filter_1(rope_m_t rp) {
  switch(rope_type(rp)) {
  case EMPTY:
    return (rope_empty ());
  case LEAF:
    if (pred(rope_elt(rp)))
      return(rope_singleton(rope_elt(rp)));
    else
      return(rope_empty ());
  case CAT: {
    rope_m_t l = rope_filter_1(rope_left(rp));
    rope_m_t r = rope_filter_1(rope_right(rp));
    return (rope_nccat2(l, r));
  }
  }
}

/* rope_balance_unst (rp) */
/* O(log n) returns balanced copy of rope rp */
/* this version of rope balancing is unstable */
rope_m_t
rope_balance_unst(rope_m_t rp) {
  switch(rope_type(rp)) {
  case EMPTY:
    return (rope_empty ());
  case LEAF:
    return(rope_singleton(rope_elt(rp)));
  case CAT: {
    long len = rope_length(rp);
    long m = len / 2;
    rope_pair_t* split = rope_nbsplit2(rp, m);
    rope_m_t bl = rope_balance_unst(split->l);
    rope_m_t br = rope_balance_unst(split->r);
    return (rope_nccat2 (bl, br));
  }}
}

/* rope_filter (rp) */
/* O(log n) applies pred to each data element of rope rp, returning
   the rope of those x for which pred x evaluated to true, in the same
   order as they occured in the argument rope */
void rope_filter(rope_m_t rp, rope_m_t* rp_d) {
  rope_m_t rp2 = rope_filter_1( rp );
  rope_m_t rp3 = rope_balance_unst( rp2 );
  *rp_d = rp3;
}


void cealtesthook_run_core() {
  core(rope_filter)(rope_input, &rope_output_core);
}

void cealtesthook_run_verf() {
  rope_filter(rope_input, &rope_output_core);
}

