/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __SCALAR_INPUT_C__
#define __SCALAR_INPUT_C__

#include "scalar.c"
#include <stdio.h>

data_t owcr      scalar_input;
long   foreign_c scalar_input_size;
long   foreign_c scalar_input_nextc;
data_t owcr      scalar_input_saved;

void cealtesthook_input_generate(long size) {
  /* Nothing. */
  scalar_input_size = size;
  scalar_input = data_rand();
}

void cealtesthook_input_print(FILE* file) {
  fprintf(file, DATA_FMT, DATA_FMT_ARGS(scalar_input));
}

void cealtesthook_input_iter_begin() {
  /* Nothing. */
  scalar_input_nextc = 0;
}

void cealtesthook_input_iter_next() {
  scalar_input_nextc += 1;
}

int cealtesthook_input_iter_isdone() {
  return scalar_input_nextc == scalar_input_size;
}

void cealtesthook_input_iter_change() {
  scalar_input_saved = scalar_input;
  scalar_input = data_rand();
}

void cealtesthook_input_iter_revert() {
  scalar_input = scalar_input_saved;
}

  
#endif
