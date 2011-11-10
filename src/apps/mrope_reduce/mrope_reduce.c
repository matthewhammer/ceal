/* Mike Rainey <mrainey@mpi-sws.org> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "mrope.c"
#include "mrope_input.c"
#include "mrope_reduce.h"
#include "main.c"

data_t output_core;
data_t output_verf;

void
cealtesthook_output_print(FILE* file, int i) {
  if(i == 0)
    fprintf(file, DATA_FMT, " ", DATA_FMT_ARGS(output_core));
  else if (i == 1)
    fprintf(file, DATA_FMT, " ", DATA_FMT_ARGS(output_verf));
  else
    abort();
}

int
cealtesthook_output_check() {
  return(DATA_EQUAL(output_core, output_verf));
}

void foobar() {
  mrope_reduce(mrope_input, SUM, 0, &output_core);
}

void cealtesthook_run_core() {
  core(foobar)();
}

void cealtesthook_run_verf() {
  mrope_reduce(mrope_input, SUM, 0, &output_verf);
}

