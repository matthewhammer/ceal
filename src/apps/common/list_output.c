/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __LIST_OUTPUT_C__
#define __LIST_OUTPUT_C__

#include <stdlib.h>
#include "list.c"

list_tl_t list_output_core;
list_tl_t list_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  if(i == 0)    
    list_fprint(file, list_output_core);
  else if (i == 1)
    list_fprint(file, list_output_verf);
  else
    abort();
}

int cealtesthook_output_check() {
  list_tl_t list_core = list_output_core;
  list_tl_t list_verf = list_output_verf;

  while(list_core && list_verf) {

    if(! DATA_EQUAL(cons_hd(list_core), cons_hd(list_verf))) {
      fprintf(stderr, __FUNCTION__
              ": not equal: " DATA_FMT " <> " DATA_FMT "\n",
              DATA_FMT_ARGS(cons_hd(list_core)),
              DATA_FMT_ARGS(cons_hd(list_verf)) );
      return 0;
    }
    
    else {
      list_core = *cons_tl(list_core);
      list_verf = *cons_tl(list_verf);
      continue;
    }
  }

  int same_length =
    ( list_core == NULL ) &&
    ( list_verf == NULL ) ;

  if(! same_length ) {
    fprintf(stderr, __FUNCTION__ ": not equal: lengths are not equal, "
            "(%p == NULL && %p == NULL) does not hold.\n",
            list_core, list_verf);
  }
  
  return same_length;
}


#endif
