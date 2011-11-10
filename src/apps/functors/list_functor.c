/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdio.h>
#include <assert.h>

#if  defined(List_hd_t) \
  && defined(Ng)
/* Then Ok. */
#else
#error Undefined functor arguments.
#endif

typedef List_hd_t        Ng(hd_t);
typedef struct Ng(cons)* Ng(tl_t);

struct Ng(cons) {
  Ng(hd_t) __hd;
  Ng(tl_t) __tl;
};

typedef struct Ng(cons)* Ng(t);

#ifndef CEAL_FOREIGN_IMMUTABLES
                  
static Ng(t) Ng(cons)(Ng(hd_t) hd) {
  Ng(t) c = alloc(struct Ng(cons));
  c->__hd = hd;
  return c;
}

static Ng(hd_t) Ng(hd)(Ng(t) list) {
  return list->__hd;
}

static Ng(t)* Ng(tl)(Ng(t) list) {
  return &(list->__tl);
}

#else

static Ng(t) Ng(cons_init) ( Ng(t) c, Ng(hd_t) hd ) foreign_c {
  c->__hd = hd;
  return c;
}

static Ng(hd_t) Ng(hd) ( Ng(t) list ) foreign_c {
  return list->__hd;
}

static Ng(tl_t)* Ng(tl) ( Ng(t) list ) foreign_c {
  return &(list->__tl);
}

static Ng(t) Ng(cons) ( Ng(hd_t) hd ) {
  return Ng(cons_init)(alloc(struct Ng(cons)), hd);
}

#endif
