/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdlib.h>
#include "cealtesthook_dummies.c"

/* The knot is a circularly-linked list, sans any data. */
typedef struct knot_s knot_t;
struct knot_s {
  knot_t* ptr;
};

knot_t* knot = NULL;
long core_runs = 0;

#define core_printf(x,...)
#define inout_printf(x,...) printf(x,##__VA_ARGS__)

/* The core program expands the knot by one unit. */
void the_core() {
  knot_t* knot0;

  core_printf("\ncore begin: \n");

  core_runs ++;

  knot0 = knot;
  knot = alloc(knot_t);
  core_printf("created %p\n", knot);

  if( knot0 ) {
    knot->ptr  = knot0->ptr;
    knot0->ptr = knot;
  }
  else
    knot->ptr = knot;

  core_printf("core done.\n");  
}

/* We walk around and print the knot. */
void print_the_knot() {
  inout_printf("after %d core runs:\n", core_runs);
  inout_printf("knot --> %p; ", knot);
  
  knot_t* ka = knot;
  if( ka ) {
    knot_t* kb = ka->ptr;
    while (kb != knot) {
      inout_printf("%p --> %p; ", ka, kb);
      ka = kb;
      kb = kb->ptr;
    }
    inout_printf("%p --> %p", ka, kb);
  }
  
  printf("\n");
}

/* If feedback is working correctly, the behavior with CP should be
   equivalent to that without CP. */
int main(int argc, char*foreign_c* argv) {

  int use_cp =
    (argc < 2) || (strcmp("-nocp", argv[1]) != 0);
  
  if( use_cp ) {
    fprintf(stderr,"using change propagation.\n");
  }
  else {
    fprintf(stderr,"not using change propagation.\n");
  }

  /* Create the ever-expanding knot. */
  knot = NULL;
  print_the_knot();

  /* Expand the knot, in one initial step. */
  if ( use_cp ) {
    core(the_core)();
    ceal_autogc_pause();
  }
  else {
    the_core();
  }

  print_the_knot();

  for(int i = 0; i < 10; i++) {
    if ( use_cp ) {
      propagate;      
    }
    else {
      the_core();      
    }

    print_the_knot();
  }

  return 0;
}
