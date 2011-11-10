/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __LIST_INPUT_C__
#define __LIST_INPUT_C__

#include "scalar.c"
#include "list.c"

#include <stdio.h>
#include <assert.h>

list_tl_t  list_input;
list_tl_t* list_iter;

static list_tl_t new_cell;
static long change_size;

void cealtesthook_input_generate(long size) {
  list_input = list_random(size);
}

void cealtesthook_input_print(FILE* file) {
  list_fprint(file, list_input);
}

void cealtesthook_input_iter_begin() {
  list_iter = &list_input;
}

void cealtesthook_input_iter_next(long change_size_) {
  change_size = change_size_;
  list_iter = cons_tl(*list_iter);
}

int cealtesthook_input_iter_isdone() {
  return (*list_iter == NULL);
}

void cealtesthook_input_iter_change() {

  assert( change_size == 1
          /* See logic in list_input_unique.c for the more general case. */ );
  
  new_cell = cons(data_rand());
  *cons_tl(new_cell) = *list_iter;
  *list_iter = new_cell;
}

void cealtesthook_input_iter_revert() {
  /* assert(*list_iter == new_cell); */
  *list_iter = *cons_tl(new_cell);
  kill(new_cell);
}


#endif
