/* Matthew Hammer <hammer@tti-c.edu> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_STRING
#include "scalar.c"

#include "list_input_unique.c"
#include "list_output.c"
#include "main.c"

static void
list_pivot(data_t pivot, list_t in, list_tl_t* d1, list_tl_t* d2) {

  if(! in ) {
    *d1 = NULL;
    *d2 = NULL;
  }
  else {
    data_t hd = cons_hd(in);
    list_t c  = memo(cons(hd));
    
    if(DATA_COMP(hd, pivot)) {
      memo;
      *d1 = c;
      list_pivot(pivot, *cons_tl(in), cons_tl(c), d2);
    }
    else {
      memo;
      *d2 = c;
      list_pivot(pivot, *cons_tl(in), d1, cons_tl(c));
    }
  }
}

static void
list_quicksort_rec(list_t in, list_t rest, list_tl_t* out) {
  if(! in ) {
    *out = rest;
  }
  else {
    data_t     pivot = cons_hd(in);
    list_t     cell;
    list_tl_t* less;
    list_tl_t* grtr;
    void*      mtbl;
    
	
    memo {
      cell = cons(pivot);
      less = alloc(list_t);
      grtr = alloc(list_t);
      mtbl = fresh_scope;
    }
    
    memo(mtbl) {
      list_pivot(pivot, *cons_tl(in), less, grtr);
    }    
    memo {
      list_quicksort_rec(*less, cell, out);
    }    
    memo {    
      list_quicksort_rec(*grtr, rest, cons_tl(cell));
    }
  }
}

static void
list_quicksort(list_t* in, list_tl_t* out) {
  list_quicksort_rec(*in, NULL, out);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void cealtesthook_run_core() {
  core(list_quicksort)(&list_input, &list_output_core);
}

void cealtesthook_run_verf() {
  list_quicksort(&list_input, &list_output_verf);
}
