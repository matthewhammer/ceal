/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdlib.h>
#include "cealtesthook_dummies.c"

typedef struct knot_s knot_t;
struct knot_s {
  knot_t* ptr;
};

knot_t* knot = NULL;

void the_core() {
  knot_t* k = knot;
  knot = alloc(knot_t);
  knot->ptr = k->ptr;
  k->ptr = knot;
}

void the_knot() {
  knot_t* ka = knot;
  knot_t* kb = ka->ptr;

  while (kb != knot && kb != ka) {
    printf("%p --> %p\n", ka, kb);
    ka = kb;
    kb = kb->ptr;
  }
  printf("%p --> %p\n", ka, kb);
}

int main(int argc, char* foreign_c* argv) {
  int use_cp =
    (argc >= 2) && !strcmp("-cp", argv[1]);
  
  if( use_cp ) {
    fprintf(stderr,"using cp.\n");
  }
  else {
    fprintf(stderr,"not using cp.\n");
  }

  knot = alloc(knot_t);
  knot->ptr = knot;

  printf("0:\n");
  the_knot();
  
  if ( use_cp ) {
    core(the_core)();
    ceal_autogc_pause();
  }
  else {
    the_core();
  }

  for(int i = 0; i < 3; i++) {
    printf("%d:\n", i+1);
    the_knot();
    
    if ( use_cp ) {
      propagate;
      
    }
    else {
      the_core();      
    }
  }

  return 0;
}
