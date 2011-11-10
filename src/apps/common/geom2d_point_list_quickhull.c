/* Matthew Hammer <hammer@tti-c.edu> */

#include <stdio.h>
#include <math.h>

#include "geom2d.c"
#include "geom2d_point_list.c"

static geom2d_point_t*
toleft(void* _, geom2d_point_t* p1, geom2d_point_t* p2) {
  return geom2d_toleft(p1, p2);
}

static geom2d_point_t*
toright(void* _, geom2d_point_t* p1, geom2d_point_t* p2) {
  return geom2d_toright(p1, p2);
}

/* Our lists are geom2d_point_list_t's */
#define  List_t       geom2d_point_list_t
#define  List_tl_t    geom2d_point_list_tl_t
#define  List_hd_t    geom2d_point_list_hd_t
#define  List_cons    cons
#define  List_hd      cons_hd
#define  List_tl      cons_tl

/* Get the furtherest point to the left. */
#define  Monoid_binop toleft
#define  Reduce_fun   list_reduce_toleft
#include "list_reduce_functor.c"
#undef   Monoid_binop
#undef   Reduce_fun

/* Get the furtherest point to the right. */
#define  Monoid_binop toright
#define  Reduce_fun   list_reduce_toright
#include "list_reduce_functor.c"
#undef   Monoid_binop
#undef   Reduce_fun

/* Get the furtherest point from the line. */
#define  Monoid_binop geom2d_maxdist
#define  Reduce_fun   list_reduce_maxdist
#include "list_reduce_functor.c"
#undef   Monoid_binop
#undef   Reduce_fun

#undef List_t
#undef List_tl_t
#undef List_hd_t
#undef List_cons

static void
quickhull_split(geom2d_point_list_tl_t points,
                geom2d_line_t* line,
                geom2d_point_list_tl_t* dest1,
                geom2d_point_list_tl_t* dest2) {
  if(points == NULL) {
    *dest1 = NULL;
    *dest2 = NULL;
  }
  else {
    geom2d_point_t* p = cons_hd(points);

    if(p == geom2d_line_p1(line) || p == geom2d_line_p2(line)) {
      quickhull_split(*cons_tl(points), line, dest1, dest2);
    }
    else {
      geom2d_point_list_t c = memo(cons(p));
    
      if(geom2d_isabove(line, p)) {
        memo;
        *dest1 = c;
        quickhull_split(*cons_tl(points), line, cons_tl(c), dest2);
      }
      else {
        memo;
        *dest2 = c;
        quickhull_split(*cons_tl(points), line, dest1, cons_tl(c));
      }
    }
  }
}

static void
quickhull_filter_rec(geom2d_line_t* line1, geom2d_point_list_tl_t* dest1,
                     geom2d_line_t* line2, geom2d_point_list_tl_t* dest2,
                     geom2d_point_list_t points) {
  
  if(points == NULL) {
    *dest1 = NULL;
    *dest2 = NULL;
  }
  else {
    geom2d_point_t* p = cons_hd(points);
    
    if(geom2d_isabove(line1, p)) {
      geom2d_point_list_t c = memo(cons(p));
      memo;
      *dest1 = c;
      quickhull_filter_rec(line1, cons_tl(c), line2, dest2, *cons_tl(points));
    }
    else if(geom2d_isabove(line2, p)) {
      geom2d_point_list_t c = memo(cons(p));
      memo;
      *dest2 = c;
      quickhull_filter_rec(line1, dest1, line2, cons_tl(c), *cons_tl(points));
    }
    else {
      memo;
      quickhull_filter_rec(line1, dest1, line2, dest2, *cons_tl(points));
    }
  }
}

static void
quickhull_filter(geom2d_line_t* line1, geom2d_point_list_tl_t* dest1,
                 geom2d_line_t* line2, geom2d_point_list_tl_t* dest2,
                 geom2d_point_list_tl_t* points) {
  memo(fresh_scope);
  quickhull_filter_rec(line1, dest1, line2, dest2, *points);
}

static void
quickhull_rec(geom2d_line_t*          line,
              geom2d_point_list_tl_t* hull,
              geom2d_point_list_tl_t* points,
              geom2d_point_list_t     rest) {

  long at_basecase = 0;

  cut {
    /* Are there fewer than 2 points? */
    geom2d_point_list_t points_ = *points;
    at_basecase =
      (points_           == NULL ||
       *cons_tl(points_) == NULL );
  }

  if(at_basecase) {
    *hull = rest;
  }
  else {
    geom2d_point_t*     mid_point = list_reduce_maxdist(points, line);
    geom2d_point_list_t mid_cons  = cons(mid_point);
    
    geom2d_line_t* left_line  = geom2d_line(geom2d_line_p1(line), mid_point);
    geom2d_line_t* right_line = geom2d_line(mid_point, geom2d_line_p2(line));

    geom2d_point_list_tl_t left_points;
    geom2d_point_list_tl_t right_points;
    
    quickhull_filter(left_line, &left_points, right_line, &right_points, points);

    quickhull_rec(left_line, hull, &left_points, mid_cons);
    quickhull_rec(right_line, cons_tl(mid_cons), &right_points, rest);
  }
}

static void
quickhull(geom2d_point_list_tl_t* points, geom2d_point_list_tl_t* hull) {

  long at_basecase = 0;

  cut {
    /* Are there fewer than 2 points? */
    geom2d_point_list_tl_t points_ = *points;
    at_basecase =
      ( points_           == NULL ||
        *cons_tl(points_) == NULL );
  }

  if( at_basecase ) {
    *hull = *points;
  }
  else {
    geom2d_point_t* min = list_reduce_toleft  (points, NULL);    
    geom2d_point_t* max = memo(list_reduce_toright (points, NULL));
    
    geom2d_line_t* upper_line = geom2d_line(min, max);
    geom2d_line_t* lower_line = geom2d_line(max, min);

    geom2d_point_list_t c1 = cons(min);
    geom2d_point_list_t c2 = cons(max);
    geom2d_point_list_t c3 = cons(min);

    geom2d_point_list_tl_t upper_points;
    geom2d_point_list_tl_t lower_points;

    memo(fresh_scope) {
      quickhull_split(*points, upper_line,
                      &upper_points, &lower_points);
    }

    quickhull_rec(upper_line, cons_tl(c1), &upper_points, c2);
    quickhull_rec(lower_line, cons_tl(c2), &lower_points, c3);
    
    *cons_tl(c3) = NULL;

    *hull = c1;
  }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Unused Stuff. */
   
static void
quickhull_filter_sparse_rec(geom2d_line_t*        line1,
                            geom2d_point_list_t** dest_dest1,
                            geom2d_line_t*        line2,
                            geom2d_point_list_t** dest_dest2,
                            geom2d_point_list_t   points) {
  
  if(points == NULL) {
    **dest_dest1 = NULL;
    **dest_dest2 = NULL;
  }
  else {
    geom2d_point_t* p = cons_hd(points);
    
    if(geom2d_isabove(line1, p)) {  
      /* Extend the first list */
      geom2d_point_list_t c = memo(cons(p));
      geom2d_point_list_t* dest = *dest_dest1;
      *dest = c;
      *dest_dest1 = cons_tl(c);
    }
    else if(geom2d_isabove(line2, p)) {  
      /* Extend the second list */
      geom2d_point_list_t c = memo(cons(p));
      geom2d_point_list_t* dest = *dest_dest2;
      *dest = c;
      *dest_dest2 = cons_tl(c);
    }

    memo;
    quickhull_filter_sparse_rec(line1, dest_dest1,
                                line2, dest_dest2,
                                *cons_tl(points));
  }
}

static void
quickhull_filter_sparse(geom2d_line_t*       line1,
                        geom2d_point_list_t* dest1,
                        geom2d_line_t*       line2,
                        geom2d_point_list_t* dest2,
                        geom2d_point_list_t* points) {

  memo(fresh_scope);
  quickhull_filter_sparse_rec(line1, &dest1,
                              line2, &dest2,
                              *points);
}
