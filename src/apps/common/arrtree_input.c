/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __ARRTREE_INPUT_C__
#define __ARRTREE_INPUT_C__

#include "arrtree.c"

#include <stdio.h>

arrtree_t   arrtree_input;

/* The iterator uses an auxililary array consisting of every pointer
     in the input that we will perform a change on.*/

arrtree_t* arrtree_input_ptrs; /* all the tree pointers we could change */
long       arrtree_input_ptrc; /* number of occupants in arrtree_input_ptrs */
long       arrtree_input_ptri; /* which pointer shall we change next? */
long       arrtree_input_arri; /* which array index should we change next? */
data_t     arrtree_orig_value; /* after a change, how do we restore the value? */

void cealtesthook_input_generate(long size) {
  arrtree_input = arrtree_random_balanced(size);

  arrtree_input_ptrc    = size * 2 + 1;
  arrtree_input_ptrs    = malloc(sizeof(arrtree_t) * arrtree_input_ptrc);
  arrtree_input_ptrs[0] = &arrtree_input;

  long ptrs_found =
    arrtree_ptrs_preorder(arrtree_input, arrtree_input_ptrs, 1);

  assert(arrtree_input_ptrc == ptrs_found);
}

void cealtesthook_input_print(FILE* file) {
  arrtree_fprint(file, arrtree_input);
}

void cealtesthook_input_iter_begin() {
  arrtree_input_ptri = 0;
  arrtree_input_arri = 0;
}

void cealtesthook_input_iter_next() {
  if(arrtree_input_arri < ARRTREE_ARRAY_LEN) {
    arrtree_input_arri = 0;
    arrtree_input_ptri ++;
  }
  else {
    arrtree_input_arri ++;
  }
}

int cealtesthook_input_iter_isdone() {
  return arrtree_input_ptri == arrtree_input_ptrc;
}

void cealtesthook_input_iter_change() {
  arrtree_orig_value =
    arrtree_input_ptrs[arrtree_input_ptri]
    ->arr[arrtree_input_arri];

  arrtree_input_ptrs[arrtree_input_ptri]
    ->arr[arrtree_input_arri]
    = data_rand();
}

void cealtesthook_input_iter_revert() {
  arrtree_input_ptrs[arrtree_input_ptri]
    ->arr[arrtree_input_arri]
    = arrtree_orig_value;
}


#endif
