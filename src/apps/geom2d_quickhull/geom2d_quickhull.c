/* Matthew Hammer <hammer@tti-c.edu> */

#include <stdio.h>
#include <math.h>

#include "geom2d_point_list_quickhull.c"
#include "geom2d_point_list_input.c"
#include "geom2d_point_list_output.c"
#include "main.c"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
quickhull_core(geom2d_point_list_tl_t* points) {
  memo(fresh_scope);
  quickhull(points, &point_list_output_core);
}

void cealtesthook_run_core() {
  core(quickhull_core)(&point_list_input);
}

void cealtesthook_run_verf() {
  quickhull(&point_list_input, &point_list_output_verf);
}


