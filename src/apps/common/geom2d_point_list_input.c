/* Matthew Hammer <hammer@ttic.edu> */

#ifndef __GEOM_2D_POINT_LIST_INPUT_H__
#define __GEOM_2D_POINT_LIST_INPUT_H__

static geom2d_point_list_tl_t  point_list_input;

void cealtesthook_input_generate(long size) {
  point_list_random_rec(size, &point_list_input);
}

void cealtesthook_input_print(FILE* file) {
  point_list_fprint(file, point_list_input);
}

static geom2d_point_list_tl_t* point_list_iter;
static long                    point_list_change_size;
static geom2d_point_list_t     point_list_removed_cells;

void cealtesthook_input_iter_begin(long change_size) {
  point_list_change_size = change_size;
  point_list_iter        = &point_list_input;  
}

void cealtesthook_input_iter_next() {
  int i = 0;  
  while ( i < point_list_change_size && *point_list_iter != NULL ) {
    point_list_iter = cons_tl( *point_list_iter );
    i++;
  }
}

int cealtesthook_input_iter_isdone() {
  return (*point_list_iter == NULL);
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
  point_list_removed_cells = *point_list_iter;
  
  /* Find the tail of the removed cells. */
  geom2d_point_list_t* tl = point_list_iter;
  int i = 0;

  /* Find the tail of the removed cells. */
  while ( i < point_list_change_size && *tl != NULL ) {
    tl = cons_tl( *tl );
    i++;
  }

  /* Remove the input cells. */    
  *point_list_iter = *tl;
}

void cealtesthook_input_iter_revert() {

  /* Re-insert the removed cells. */
  *point_list_iter = point_list_removed_cells;
}

#endif
