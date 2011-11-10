/* Matthew Hammer <hammer@tti-c.edu> */

#include <stdio.h>
#include <math.h>

/* Our input is lists of 2d points.
   We use quickhull as a subroutine. */
#include "geom2d.c"
#include "geom2d_point_list.c"
#include "geom2d_point_list_input.c"
#include "geom2d_point_list_quickhull.c"

#undef List_hd
#undef List_tl

/* We use lists of doubles as intermediate results. */
#define List_hd_t double
#define Ng(name)  double_list_##name
#include "list_functor.c"
#undef List_hd_t
#undef Ng

/* Output (the diameter) is a double. */
#include "scalar_types.c"
#define  SCALAR_DATA SCALAR_DOUBLE
#include "scalar.c"
#include "scalar_output.c"
#include "main.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Compute a list of all point-to-point distances. */

static double_list_t
cons_distance_(geom2d_point_t* p1, geom2d_point_t* p2) {
  return double_list_cons(geom2d_dist(p1, p2));
}

static double_list_t
cons_distance(geom2d_point_t* p1, geom2d_point_t* p2) {
  memo;
  return double_list_cons(geom2d_dist(p1, p2));
}

#define  List_in_t      geom2d_point_list_t
#define  List_in_tl_t   geom2d_point_list_tl_t
#define  List_in_hd     cons_hd
#define  List_in_tl     cons_tl
#define  List_out_t     double_list_t
#define  List_out_tl_t  double_list_tl_t
#define  List_out_cons  cons_distance
#define  List_out_tl    double_list_tl
#define  List_cross     all_distances
#include "list_cross_functor.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Find the biggest double in a list. */

static double max_distance(void* _, double d, double q) {
  return d > q ? d : q;
}

#define  List_t       double_list_t
#define  List_tl_t    double_list_tl_t
#define  List_hd_t    double_list_hd_t
#define  List_cons    double_list_cons
#define  List_hd      double_list_hd
#define  List_tl      double_list_tl
#define  Monoid_binop max_distance
#define  Reduce_fun   reduce_max_distance
#include "list_reduce_functor.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void diameter(geom2d_point_list_tl_t* points, double owcr* d) {
  geom2d_point_list_tl_t hull;
  double_list_tl_t       dists;
  
  quickhull(points, &hull);
  
  all_distances(hull, &hull, &dists);
  
  *d = reduce_max_distance(&dists, NULL);
}

static void
diameter_core(geom2d_point_list_tl_t* points) {
  memo(fresh_scope);
  diameter(points, &scalar_output_core);
}

void cealtesthook_run_core() {
  core(diameter_core)(&point_list_input);
}

void cealtesthook_run_verf() {
  diameter(&point_list_input, &scalar_output_verf);
}
