/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "list_input_unique.c"
#include "scalar_output.c"
#include "main.c"

static data_t
min(void* _, data_t a, data_t b) {
  return data_min(a, b);
}

#define List_t       list_t
#define List_tl_t    list_tl_t
#define List_hd_t    list_hd_t
#define List_cons    cons
#define List_hd      cons_hd
#define List_tl      cons_tl
#define Monoid_binop min
#define Reduce_fun   list_min
#include "list_reduce_functor.c"

static void list_min_core(list_t* in, data_t owcr* d) {
  *d = list_min(in, NULL);
}

void cealtesthook_run_core() {
  core(list_min_core)(&list_input, &scalar_output_core);
}

void cealtesthook_run_verf() {
  list_min_core(&list_input, &scalar_output_verf);
}
