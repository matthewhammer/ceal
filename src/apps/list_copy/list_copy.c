/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_STRING
#include "scalar.c"

#include "list_input_unique.c"
#include "list_output.c"
#include "main.c"


static void
list_copy(list_t in, list_tl_t* d) {
  if(! in ) {
    *d = NULL;
  }
  else {
    data_t hd = cons_hd(in);
    list_t c  = memo(cons(hd));
    memo;
    *d = c;
    list_copy(*cons_tl(in), cons_tl(c));
  }
}

static void
list_copy_core(list_tl_t* in, list_tl_t* d) {
  //memo(fresh_scope);
  list_copy(*in, d);
}

void cealtesthook_run_core() {
  core(list_copy_core)(&list_input, &list_output_core);
}

void cealtesthook_run_verf() {
  list_copy(list_input, &list_output_verf);
}
