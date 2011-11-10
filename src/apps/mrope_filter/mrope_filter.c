/* Mike Rainey <mrainey@mpi-sws.org> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "mrope.c"
#include "mrope_input.c"
#include "mrope_output.c"
#include "mrope_filter.h"
#include "main.c"

/*
mrope_t mrope_input;

static long mrope_iter;
static long mrope_size;
static data_t mrope_orig_leaf_elt;

void cealtesthook_input_generate(long size) {
  mrope_input = mrope_nbcat2(mrope_singleton(5), mrope_singleton(3));
}

void cealtesthook_input_print(FILE* file) {
  mrope_fprint(file, mrope_input);
}

void cealtesthook_input_iter_begin() {
  mrope_iter = 0;
}

void cealtesthook_input_iter_next() {
  mrope_iter ++;
}

int cealtesthook_input_iter_isdone() {
  return mrope_iter >= mrope_size - 1;
}
void cealtesthook_input_iter_change() {
  mrope_t* r = mrope_addr_right(mrope_input);
  *r = mrope_nbcat2(mrope_singleton(1), mrope_singleton(4));  
}

void cealtesthook_input_iter_revert() {

}
*/

#define PIVOT 20

void foobar() {
  mrope_filter(mrope_input, PIVOT, GREATER, &mrope_output_core);
}

void cealtesthook_run_core() {
  core(foobar)();
}

void cealtesthook_run_verf() {
  mrope_filter(mrope_input, PIVOT, GREATER, &mrope_output_verf);
}

