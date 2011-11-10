/* Mike Rainey <mrainey@mpi-sws.org>   */
/* Matthew Hammer <hammer@mpi-sws.org> */

#ifndef __ROPE_INPUT_C__
#define __ROPE_INPUT_C__

#include <stdio.h>
#include <assert.h>

rope_m_t rope_input;

static long rope_iter;
static long rope_size;
static data_t rope_orig_leaf_elt;

void cealtesthook_input_generate(long size) {
  rope_input = rope_random(size);
  rope_size = size;
}

void cealtesthook_input_print(FILE* file) {
  rope_fprint(file, rope_input);
}

void cealtesthook_input_iter_begin() {
  rope_iter = 0;
}

void cealtesthook_input_iter_next() {
  rope_iter ++;
}

int cealtesthook_input_iter_isdone() {
  return rope_iter >= rope_size - 1;
}

void cealtesthook_input_iter_change() {
  rope_m_t rp = rope_sub_ib_node(rope_input, rope_iter);
  assert(rope_type(rp) == LEAF);
  rope_orig_leaf_elt = rp->node_u.elt;
  rp->node_u.elt     = data_rand();
}

void cealtesthook_input_iter_revert() {
  rope_m_t rp = rope_sub_ib_node(rope_input, rope_iter);
  assert(rope_type(rp) == LEAF);
  rp->node_u.elt = rope_orig_leaf_elt;
}

#endif
