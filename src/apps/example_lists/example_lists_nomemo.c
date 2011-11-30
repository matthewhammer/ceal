/* Matthew Hammer <hammer@mpi-sws.org> */

/* Type definitions. */
typedef struct cons_s cons_t;
typedef cons_t* list_t;
struct cons_s {
  data_t hd;
  list_t tl;
} list_t;

/* List construction. */

/* -- nil constructor. */
#define nil ((list_t) NULL)

/* -- cons constructor.
   (Tail field to be filled in later.) */
list_t list_cons(data_t d) {
  list_t cons = alloc( cons_t );
  cons->hd = d;
  return cons;
}

/* -- cons constructor.
   (Tail field filled in during construction.) */
list_t list_cons_(data_t hd, list_t tl) {
  cons_t* c = list_cons(hd);
  c->tl = tl;
  return c;
}

void list_print_rec(list_t in) {
  if( in ) {
    printf("%ld", in->hd);
    if( in->tl ) {
      printf(", ");
      list_print_rec(in->tl);
    }
  }
}

void list_print(const char* name, list_t in) {
  printf("%s: [", name);
  list_print_rec( in );
  printf("]\n");
}

data_t some_function(data_t d) {
  return labs(d);
}

long some_predicate(data_t d) {
  return labs(d) == d;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - */
/* TODO:
   1. Copy this file to example_lists_<username>.c
   2. svn add example_lists_<username>.c
   3. In your copy, insert cuts/memos below as needed.
*/

void list_map(list_t in, list_t* out) { db("");  
  if(in == nil) {
    *out = nil;
  }
  else {
    data_t new_hd = some_function( in->hd );
    list_t new_cons = list_cons( new_hd );
    *out = new_cons;
    list_map ( in->tl, & new_cons->tl );
  }  
}

void list_filter(list_t in, list_t* out) { db("");
  if(in == nil) {
    *out = nil;
  }
  else {
    if( some_predicate( in->hd ) ) {
      list_t new_cons = list_cons( in->hd );
      *out = new_cons;
      list_filter ( in->tl, & new_cons->tl );
    }
    else {
      list_filter ( in->tl, out );
    }
  }
}

void list_split(list_t in, list_t* out1, list_t* out2) { db("");
  if(in == nil) {
    *out1 = nil;
    *out2 = nil;
  }
  else {
    list_t new_cons = list_cons( in->hd );
    
    if( some_predicate( in->hd ) ) {
      *out1 = new_cons;
      list_split ( in->tl, & new_cons->tl, out2 );
    }
    else {
      *out2 = new_cons;
      list_split ( in->tl, out1, & new_cons->tl );
    }
  }
}

void list_reverse(list_t in, list_t rev, list_t* out) { db("");
  if( in == nil ) {
    *out = rev;
  }
  else {
    list_t new_cons = list_cons( in->hd );
    new_cons->tl = rev;
    list_reverse (in->tl, new_cons, out);
  }
}
