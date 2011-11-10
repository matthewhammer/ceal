/* Mike Rainey <mrainey@mpi-sws.org>   */
/* Matthew Hammer <hammer@mpi-sws.org> */

#ifndef __MROPE_INPUT_C__
#define __MROPE_INPUT_C__

#include <stdio.h>
#include <assert.h>

mrope_t mrope_input;

static long mrope_iter;
static long mrope_size;
static data_t mrope_orig_leaf_elt;

void cealtesthook_input_generate(long size) {
  mrope_input = mrope_random(size);
  mrope_size = size;
}

void cealtesthook_input_print(FILE* file) {
  mrope_fprint(file, mrope_input);
}

void cealtesthook_input_iter_begin() {
  mrope_iter = 0;
  //  mrope_iter = mrope_size / 2;
}

void cealtesthook_input_iter_next() {
  mrope_iter ++;
}

int cealtesthook_input_iter_isdone() {
  return mrope_iter >= mrope_size - 1;
}

void
change_elt() {
  mrope_t m = mrope_sub_ib_node(mrope_input, mrope_iter);
  assert(mrope_node(m) == LEAF);
  mrope_orig_leaf_elt = m->node_u.leaf_s.d;
  m->node_u.leaf_s.d  = mrope_rand_data();
}
void
unchange_elt() {
  mrope_t m = mrope_sub_ib_node(mrope_input, mrope_iter);
  assert(mrope_node(m) == LEAF);
  m->node_u.leaf_s.d = mrope_orig_leaf_elt;
}

void
ins_elt() {
  //  printf("mrope_insert1 depth = %d length = %d\n",mrope_depth(mrope_input), mrope_length(mrope_input));
  mrope_insert_ib(&mrope_input, 0, 99999l);
  //  printf("mrope_insert2 depth = %d length = %d\n",mrope_depth(mrope_input), mrope_length(mrope_input));
  //  mrope_fprint(stdout, mrope_input);
  //  printf("\n");
}

void cealtesthook_input_iter_change() {
  //  change_elt();
  ins_elt();
}

void cealtesthook_input_iter_revert() {
  //  unchange_elt();
}

#endif
