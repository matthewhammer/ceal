/* Matthew Hammer <hammer@ttic.edu> */

#ifndef __GEOM_2D_POINT_LIST_INPUT_TWO_H__
#define __GEOM_2D_POINT_LIST_INPUT_TWO_H__

geom2d_point_list_tl_t  point_list_input_1;
geom2d_point_list_tl_t  point_list_input_2;
geom2d_point_list_tl_t* point_list_iter;
long                    point_list_iter_cnt;
geom2d_point_list_tl_t  point_list_rem_cells;
long                    point_list_change_size;

void cealtesthook_input_generate(long size) {
  point_list_random_rec(size / 2, &point_list_input_1);
  point_list_random_rec(size / 2, &point_list_input_2);
}

void cealtesthook_input_print(FILE* file) {
  point_list_fprint(file, point_list_input_1);
  fprintf(file, "\n");
  point_list_fprint(file, point_list_input_2);
}

void cealtesthook_input_iter_begin(long change_size) {
  point_list_iter        = &point_list_input_1;
  point_list_iter_cnt    = 1;
  point_list_change_size = change_size;
}

void cealtesthook_input_iter_next() {
  int i = 0;  
  while ( i < point_list_change_size && *point_list_iter != NULL ) {
    point_list_iter = cons_tl( *point_list_iter );
    i++;
  }
  
  if( *point_list_iter    == NULL &&
      point_list_iter_cnt == 1       )
  {
    point_list_iter     = &point_list_input_2;
    point_list_iter_cnt = 2;
  }
}

int cealtesthook_input_iter_isdone() {
  return
    ( *point_list_iter     == NULL ) &&
    (  point_list_iter_cnt == 2    );
}

void cealtesthook_input_iter_change() {
    
  /* Save a pointer to removed cells. */
  point_list_rem_cells = *point_list_iter;
  
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
  *point_list_iter = point_list_rem_cells;
}

#endif
