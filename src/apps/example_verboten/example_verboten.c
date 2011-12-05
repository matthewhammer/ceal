/* Matthew Hammer <hammer@mpi-sws.org> */
#include <stdlib.h>
#include "cealtesthook_dummies.c"

typedef struct knot_s knot_t;
struct knot_s {
  knot_t* knot;
  
};

knot_t* knot = NULL;

void the_core() {
  knot_t* knot0 = knot;
  knot = alloc(knot_t);
  knot->knot = knot0->knot;
  knot0->knot = knot;
}

int main(int argc, char** argv) {
  knot = alloc(knot_t);
  knot->knot = knot;

  core(the_core)();
  ceal_autogc_pause();  
  propagate;
  knot = knot;
  propagate;
  knot = knot;
  propagate;

  return 1;
}
