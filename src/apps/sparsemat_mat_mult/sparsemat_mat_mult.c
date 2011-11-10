/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_DOUBLE
#include "scalar.c"

#include "sparsemat_input.c"
#include "sparsemat_output.c"
#include "main.c"

void sparsemat_mat_mult_core(sparsemat_t* in1,
                             sparsemat_t* in2,
                             sparsemat_t* out) {
  memo(fresh_scope);
  *out = sparsemat_mat_mult(*in1, *in2);
}

void cealtesthook_run_core() {
  core(sparsemat_mat_mult_core)(&sparsemat_input1,
                                &sparsemat_input2,
                                &sparsemat_output_core);
}

void cealtesthook_run_verf() {
  sparsemat_output_verf =
    sparsemat_mat_mult(sparsemat_input1,
                       sparsemat_input2);
}
