/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"
#include "scalar_input.c"
#include "scalar_output.c"
#include "main.c"

static void test_copy(long owcr * in, long owcr * out) {
  *out = *in;
}

void cealtesthook_run_core() {
  core(test_copy)(&scalar_input, &scalar_output_core);
}

void cealtesthook_run_verf() {
  test_copy(&scalar_input, &scalar_output_verf);
}
