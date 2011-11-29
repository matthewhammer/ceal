/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdio.h>
#include <stdlib.h>
#include "cealtesthook_dummies.c"

#define print(FMT,...) printf("%s:%d: " FMT "\n", __FUNCTION__,__LINE__,##__VA_ARGS__)

typedef long data_t;

/* Definition of lists. */

typedef struct cons_s cons_t;

typedef cons_t* list_t;

struct cons_s {
  data_t hd;
  list_t tl;
} list_t;

/* List construction. */

#define nil ((list_t) NULL)

list_t cons(data_t d) {
  list_t cons = alloc( cons_t );
  cons->hd = d;
  return cons;
}


data_t some_function(data_t d) {
  return labs(d);
}

void map(list_t in, list_t* out) {
  if(in == nil) {
    *out = nil;
  }
  else {
    data_t new_hd = some_function( in->hd );
    list_t new_cons = cons( new_hd );
    *out = new_cons;
    map( in->tl, & new_cons->tl );
  }  
}


long some_predicate(data_t d) {
  return labs(d) == d;
}

void filter(list_t in, list_t* out) {
  if(in == nil) {
    *out = nil;
  }
  else {
    if( some_predicate( in->hd ) ) {
      list_t new_cons = cons( in->hd );
      *out = new_cons;
      filter( in->tl, & new_cons->tl );
    }
    else {
      filter( in->tl, out );
    }
  }
}

void split(list_t in, list_t* out1, list_t* out2) {
  if(in == nil) {
    *out = nil;
  }
  else {
    list_t new_cons = cons( in->hd );
    
    if( some_predicate( in->hd ) ) {
      *out1 = new_cons;
      split( in->tl, & new_cons->tl, out2 );
    }
    else {
      *out2 = new_cons;
      split( in->tl, out1, & new_cons->tl );
    }
  }
}

void reverse(list_t in, list_t rev, list_t* out) {
  if( in == nil ) {
    *out = rev;
  }
  else {
    list_t new_cons = cons( in->hd );
    new_cons->tl = rev;
    reverse(in->tl, new_cons, out);
  }
}


list_t cons_(data_t hd, list_t tl) {
  cons_t c = cons(hd);
  c->tl = tl;
  return c;
}

void list_print(list_t in) {
  printf("[");
  printf("%ld", in->hd);
  if(in->tl) {
    printf(",");
    list_print(in->tl);
  }
  else {
    printf("]");
  }  
}

  
map(in, &out1); list_print(out1);


int main() {

  list_t in =
    cons_(1, cons_(2, cons_(3, cons_(4, cons_(5, nil)))));

  list_t out1;
  list_t out2;
  list_t out3;

  
  
  return 0;
}
