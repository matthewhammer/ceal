/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"
#include "scalar_input.c"
#include "scalar_output.c"
#include "main.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
static long bar (long owcr* x) {
  return *x;
}

/* This function exhibits the 'missing rvars bug'. */
static long baz (long owcr* x) {
  memo;
  /*return bar(x);*/
  return *x;
}

/* This function exhibits the 'missing rlets bug'. */
static long owcr* foo (long owcr* x) {
  memo;
  long owcr* p = alloc(long owcr);
  *p = baz(x);
  return p;
}

static void test_rvars(long owcr * in, long owcr * out) {
  memo(fresh_scope);
  *out = *(foo(in));
}

static void test_copy(long owcr * in, long owcr * out) {
  *out = *in;
}

void cealtesthook_run_core() {
  core(test_rvars)(&scalar_input, &scalar_output_core);
}

void cealtesthook_run_verf() {
  test_rvars(&scalar_input, &scalar_output_verf);
}
