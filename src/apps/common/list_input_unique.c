/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __LIST_INPUT_C__
#define __LIST_INPUT_C__

#include "scalar.c"
#include "list.c"

#include <stdio.h>

static list_tl_t list_input;

void cealtesthook_input_generate(long size) {
  data_t* array = malloc(sizeof(data_t) * size);
  memset(array, 0, sizeof(data_t) * size);
  
  /* Step 1: generate 'size' number of (unique) data elements.  Each
     such unique element is "named" (indexed) by a number i. */
  {int i; for (i = 0; i < size; i++) {
      array[i] = data_ith(i);
    }}

  /* Step 2: permute the array so that it is uniformly random. */
  {int i; for (i = 0; i < size; i++) {
      /* Choose random index in range [i, size - 1] */
      int j = i + (rand() % (size - i));
      assert(j >= i);
      assert(j < size);

      /* Swap the ith and jth entries: */ {
        data_t ith = array[i];
        array[i]   = array[j];
        array[j]   = ith;
      }
    }}
  
  list_input = list_from_array(array, size);
}

void cealtesthook_input_print(FILE* file) {
  list_fprint(file, list_input);
}


static list_tl_t* list_iter;
static long       change_size;
static list_t     removed_cells;

void cealtesthook_input_iter_begin(long change_size_) {
  change_size = change_size_;
  list_iter = &list_input;
}

void cealtesthook_input_iter_next() {
  int i = 0;  
  while ( i < change_size && *list_iter != NULL ) {
    list_iter = cons_tl( *list_iter );
    i++;
  }  
}

int cealtesthook_input_iter_isdone() {
  return (*list_iter == NULL);
}

/* OLD WAY:
   change=insert-random, revert=remove-inserted

   NEW WAY (April 1, 2011 -- No, not a joke.):
   change=remove-current, revert=insert-current

   (YET-ANOTHER) NEW WAY (Sept 19, 2011):
   allow for change_size >= 1, (instead of change-size = 1).
*/
void cealtesthook_input_iter_change() {

  /* Save a pointer to removed cells. */
  removed_cells = *list_iter;

  /* Find the tail of the removed cells. */
  list_t* tl = list_iter;
  int i = 0;
  
  while ( i < change_size && *tl != NULL ) {
    tl = cons_tl( *tl );
    i++;
  }

  /* Remove the input cells. */  
  *list_iter = *tl;
}

void cealtesthook_input_iter_revert() {

  /* Re-insert the removed cells. */
  *list_iter = removed_cells;
}


#endif
