/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_DOUBLE
#include "scalar.c"

#include "sparsemat2_input.c"
#include "sparsemat2_output.c"
#include "main.c"

void my_sparsemat_transpose(sparsemat_t* in, sparsemat_t* out) {
  *out = sparsemat_transpose(*in);
}

void cealtesthook_run_core() {
  core(my_sparsemat_transpose)(&sparsemat_input1, &sparsemat_output_core);
}

void cealtesthook_run_verf() {
  my_sparsemat_transpose(&sparsemat_input1, &sparsemat_output_verf);
}
