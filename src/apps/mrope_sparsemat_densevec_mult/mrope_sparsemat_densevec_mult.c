/* Mike Rainey <mrainey@mpi-sws.org> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "mrope.c"
#include "mrope_sparsemat.c"
#include "mrope_sparsemat_input.c"
#include "mrope_sparsemat_output.c"
#include "main.c"

void
doit() {
  sparsemat_densevec_mult(mtx, vec, 0, densevec_output_core);
}

void cealtesthook_run_core() {
  densevec_output_core = densevec_random(nrows);
  core(doit)();
}

void cealtesthook_run_verf() {
  densevec_output_verf = densevec_random(nrows);
  sparsemat_densevec_mult(mtx, vec, 0, densevec_output_verf);
}
