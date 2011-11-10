/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_DOUBLE
#include "scalar.c"

#include "arrtree_input.c"
#include "scalar_output.c"
#include "main.c"

static data_t
sum(void* _, data_t a, data_t b) {
  return data_add(a, b);
}

#define  Monoid_t   data_t
#define  List_t     list_t
#define  Monoid_fun sum

#define  Reduce_fun array_sum
#include "array_reduce.c"

#define  Array_fun  array_sum
#define  Reduce_fun arrtree_sum
#include "arrtree_reduce.c"

static void arrtree_sum_core(arrtree_t* in, data_t* d) {
  *d = arrtree_sum(*in, NULL, 0);
}

void cealtesthook_run_core() {
  core(arrtree_sum_core)(&arrtree_input, &scalar_output_core);
}

void cealtesthook_run_verf() {
  arrtree_sum_core(&arrtree_input, &scalar_output_verf);
}
