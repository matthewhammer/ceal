/* Matthew Hammer <hammer@ttic.edu> */

#ifndef __GEOM_2D_POINT_LIST_H__
#define __GEOM_2D_POINT_LIST_H__

#include <stdlib.h>
#include "geom2d.c"
#include "random.h"

typedef struct geom2d_point_cons_s  geom2d_point_cons_t;
typedef geom2d_point_cons_t*        geom2d_point_list_t;
typedef geom2d_point_list_t owcr    geom2d_point_list_tl_t;
typedef geom2d_point_t*     zwzr    geom2d_point_list_hd_t;

struct geom2d_point_cons_s {
  geom2d_point_list_hd_t __hd;
  geom2d_point_list_tl_t __tl;
};


/* - - - - - - - - - - - - - - - - - - - - - - - - */
#ifndef CEAL_FOREIGN_IMMUTABLES

static geom2d_point_list_t
cons(geom2d_point_t* pt) {
  geom2d_point_list_t c = alloc(struct geom2d_point_cons_s);
  c->__hd = pt;
  return c;
}


#define cons_hd(c) ({ (c)->__hd; })
#define cons_tl(c) ({ &((c)->__tl); })

/* - - - - - - - - - - - - - - - - - - - - - - - - */
#else

void cons_init(geom2d_point_cons_t* c,
               geom2d_point_list_hd_t p) foreign_c {
  c->__hd = p;
}

#define cons(hd) ({                                         \
      geom2d_point_cons_t* c = alloc(geom2d_point_cons_t);  \
      cons_init(c, hd); c; })

geom2d_point_list_hd_t cons_hd(geom2d_point_cons_t* c) foreign_c {
  return c->__hd;
}
  
geom2d_point_list_tl_t* cons_tl(geom2d_point_cons_t* c) foreign_c {
  return &(c->__tl);
}

#endif
/* - - - - - - - - - - - - - - - - - - - - - - - - */



static geom2d_point_t* random_point() {
  return geom2d_point(random_double(), random_double());
}

static void
point_list_random_rec(long len, geom2d_point_list_tl_t* d) {
  if(len <= 0)
    *d = NULL;
  else {
    geom2d_point_list_t c = cons(random_point());
    *d = c;
    point_list_random_rec(len - 1, cons_tl(c));
  }
}

static void
point_list_fprint(FILE* file, geom2d_point_list_tl_t list) {
  fprintf(file, "[");
  
  while(list) {
    geom2d_point_print(cons_hd(list), file);
    list = *cons_tl(list);
    if(list)
      fprintf(file, ", ");
  }  
  fprintf(file, "]");
}


#endif
