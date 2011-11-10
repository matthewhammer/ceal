/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_DOUBLE
#include "scalar.c"

#include "sparsemat2_input.c"
#include "sparsevec2_output.c"
#include "main.c"

void my_sparsemat_vec_mult(sparsemat_t* m,
                           sparsevec_t* v,
                           sparsevec_t* w) {
  sparsemat_vec_mult(*m, *v, w);
}

void cealtesthook_run_core() {
  sparsevec_output_size = sparsemat_input1->rowc;
  core(my_sparsemat_vec_mult)(&sparsemat_input1,
                              &sparsevec_input,
                              &sparsevec_output_core);
}

void cealtesthook_run_verf() {
  sparsevec_output_size = sparsemat_input1->rowc;
  my_sparsemat_vec_mult(&sparsemat_input1,
                        &sparsevec_input,
                        &sparsevec_output_verf);
}
