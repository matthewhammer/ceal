/* Mike Rainey <mrainey@mpi-sws.org> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "mrope.c"
#include "mrope_input.c"
#include "mrope_output.c"
#include "mrope_filter.h"
#include "main.c"

static mrope_t
mrope_dummy() {
  return(mrope_singleton(0));
}

void
mrope_quicksort(mrope_t m, mrope_t* d) {
  mrope_t hd = mrope_head(m);
  if (mrope_is_empty(hd)) {
    *d = mrope_empty();
  } else {
    *d = mrope_nbcat2(mrope_dummy(), 
	      mrope_nbcat2(mrope_dummy(), mrope_dummy()));

    data_t p = mrope_data(hd);
    mrope_t l;
    mrope_t g;

    mrope_filter(m, p, EQUAL, mrope_addr_left(mrope_right(*d)));
    mrope_filter(m, p, LESS, &l);
    mrope_filter(m, p, GREATER, &g);

    mrope_quicksort(l, mrope_addr_left(*d));
    mrope_quicksort(g, mrope_addr_right(mrope_right(*d)));

    mrope_finalize(mrope_addr_right(*d));
    mrope_finalize(d);

  }
}

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
  return mrope_iter >= 3;
}

void cealtesthook_input_iter_change() {
  if(mrope_iter == 0) {
    mrope_t* r = mrope_addr_right(mrope_input);
    *r = mrope_nbcat2(mrope_right(mrope_input), mrope_singleton(6));
    fprintf(stdout, "change %d\n", mrope_iter);
  } else if (mrope_iter == 1) {
    mrope_t* r = mrope_addr_right(mrope_right(mrope_input));
    *r = mrope_nbcat2(mrope_right(mrope_right(mrope_input)), mrope_singleton(7));
    fprintf(stdout, "change %d\n", mrope_iter);
  } else if (mrope_iter == 2) {
    mrope_input = mrope_nbcat2(mrope_input, mrope_singleton(1));
    fprintf(stdout, "change %d\n", mrope_iter);
  }
}

void cealtesthook_input_iter_revert() {
  
}
*/

void
mrope_quicksort_test(mrope_t m, mrope_t* d) {
  //  memo(fresh_scope);
  mrope_quicksort(m, d);
}

void cealtesthook_run_core() {
  core(mrope_quicksort_test)(mrope_input, &mrope_output_core);
}

void cealtesthook_run_verf() {
  mrope_quicksort_test(mrope_input, &mrope_output_verf);
}
