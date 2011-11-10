/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_STRING
#include "scalar.c"

#include "list_input_unique.c"
#include "list_output.c"
#include "main.c"

#define List_t      list_t
#define List_tl_t   list_tl_t
#define List_hd_t   list_hd_t
#define List_cons   cons
#define List_hd     cons_hd
#define List_tl     cons_tl

#define Hd_less_than(a,b) DATA_COMP(a,b)
#define Ng(name)          my_##name

#include "list_mergesort_functor.c"

void cealtesthook_run_core() {
  core(my_mergesort)(&list_input, &list_output_core);
}

void cealtesthook_run_verf() {
  my_mergesort(&list_input, &list_output_verf);
}
