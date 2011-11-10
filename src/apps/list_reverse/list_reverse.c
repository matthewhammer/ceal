/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "list_input_unique.c"
#include "list_output.c"
#include "main.c"

static void
list_reverse(list_t in, list_t rev, list_tl_t* d) {
  if(! in ) {
    *d = rev;
  }
  else {
    data_t hd = cons_hd(in);
    list_t c = memo(cons(hd));
    *cons_tl(c) = rev;
    memo;
    list_reverse(*cons_tl(in), c, d);
  }
}

static void
list_reverse_core(list_tl_t* in, list_tl_t* d) {
  memo(fresh_scope);
  list_reverse(*in, NULL, d);
}

void cealtesthook_run_core() {
  core(list_reverse_core)(&list_input, &list_output_core);
}

void cealtesthook_run_verf() {
  list_reverse(list_input, NULL, &list_output_verf);
}
