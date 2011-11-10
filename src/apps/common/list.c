/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __LIST_C__
#define __LIST_C__

#include <stdio.h>
#include <assert.h>

/* List datatype */

typedef struct cons_s cons_t;
typedef cons_t*       list_t;
typedef list_t owcr   list_tl_t;
typedef data_t zwzr   list_hd_t;

struct cons_s {
  list_hd_t __hd;
  list_tl_t __tl;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - */
#ifndef CEAL_FOREIGN_IMMUTABLES

static list_t cons(data_t hd) {
  cons_t* c = alloc(cons_t);
  c->__hd = hd;
  return c;
}

#define cons_hd(c) ({ (c)->__hd; })
#define cons_tl(c) ({ &((c)->__tl); })

/* - - - - - - - - - - - - - - - - - - - - - - - - */
#else

static void cons_init(cons_t* c, data_t hd) foreign_c {
  c->__hd = hd;
}

#define cons(hd) ({                   \
  struct cons_s* c = alloc(cons_t);   \
  cons_init(c, hd); c; })

static data_t cons_hd(cons_t* c) foreign_c {
  return c->__hd;
}
  
static list_tl_t* cons_tl(cons_t* c) foreign_c {
  return &(c->__tl);
}

#endif
/* - - - - - - - - - - - - - - - - - - - - - - - - */


static void
list_random_rec(int len, list_tl_t* d) {
  if(len <= 0)
    *d = NULL;
  else {
    list_t c = cons(data_rand());
    *d = c;
    list_random_rec(len - 1, cons_tl(c));
  }
}

static list_t
list_random(int len) {
  list_tl_t list;
  list_random_rec(len, &list);
  return list;
}

static void
list_ordered_rec(int len, long i, list_tl_t* d) {
  if(len <= 0)
    *d = NULL;
  else {
    list_t c = cons(data_ith(i));
    *d = c;
    list_ordered_rec(len - 1, i + 1, cons_tl(c));
  }
}

static list_t
list_ordered(int len) {
  list_tl_t list;
  list_ordered_rec(len, 0, &list);
  return list;
}

static void
list_from_array_rec(data_t* array, int i, int len, list_tl_t* d) {  
  if(i == len)
    *d = NULL;
  else {
    list_t c = cons(array[i]);
    *d = c;
    list_from_array_rec(array, i + 1, len, cons_tl(c));
  }
}

static list_t
list_from_array(data_t* array, int len) {
  list_tl_t list;
  list_from_array_rec(array, 0, len, &list);
  return list;
}

static void
list_fprint(FILE* file, list_t list_) {
  fprintf(file, "[");

  long index = 0;
  list_tl_t list = list_;
  
  while(list) {
    fprintf(file, DATA_FMT, DATA_FMT_ARGS(cons_hd(list)));
    list = *cons_tl(list);
    index ++;    
    if(list)
      fprintf(file, ", ");
  }  
  fprintf(file, "]");
}

/*
static void
list_check_sorted(list_t list) {
  list_t last = NULL;
  
  while(list) {

    if(last) {
      assert( DATA_COMP(last->hd, list->hd) );
    }
    
    last = list;
    list = list->tl;
  }  
}

static void
list_check_equal(list_t in, list_t out) {

  while( in ) {
    assert ( out );
    assert ( in->hd == out->hd );

    in  = in->tl ;
    out = out->tl ;
  }
  
  assert ( out == NULL );
}
*/

#endif
