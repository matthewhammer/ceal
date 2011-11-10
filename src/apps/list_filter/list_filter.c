/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "list_input_unique.c"
#include "list_output.c"
#include "main.c"

static void
list_filter(list_t in, list_tl_t* d) {
  if(! in ) {
    *d = NULL;
  }
  else {
    data_t hd = cons_hd(in);

    /* Somewhat arbitrary filtering predicate. */
    if( ((hd / 3) + (hd / 7) + (hd / 9)) % 2 ) {
      list_t c = memo(cons(hd));
      memo;
      *d = c;
      list_filter(*cons_tl(in), cons_tl(c));
    }
    else {
      memo;
      list_filter(*cons_tl(in), d);
    }
  }
}

static void
list_filter_core(list_tl_t* in, list_tl_t* d) {
  memo(fresh_scope);
  list_filter(*in, d);
}

void cealtesthook_run_core() {
  core(list_filter_core)(&list_input, &list_output_core);
}

void cealtesthook_run_verf() {
  list_filter(list_input, &list_output_verf);
}
