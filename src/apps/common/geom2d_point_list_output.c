/* Matthew Hammer <hammer@ttic.edu> */

#ifndef __GEOM_2D_POINT_LIST_OUTPUT_H__
#define __GEOM_2D_POINT_LIST_OUTPUT_H__

geom2d_point_list_tl_t point_list_output_core;
geom2d_point_list_tl_t point_list_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  if(i == 0)    
    point_list_fprint(file, point_list_output_core);
  else if (i == 1)
    point_list_fprint(file, point_list_output_verf);
  else
    abort();
}

int cealtesthook_output_check() {
  geom2d_point_list_tl_t list_core = point_list_output_core;
  geom2d_point_list_tl_t list_verf = point_list_output_verf;

  while(list_core && list_verf) {

    if(cons_hd(list_core) != cons_hd(list_verf))
      return 0;
    
    else {
      list_core = *cons_tl(list_core);
      list_verf = *cons_tl(list_verf);
      continue;
    }
  }

  return
    ( list_core == NULL ) &&
    ( list_verf == NULL ) ;  
}

#endif
