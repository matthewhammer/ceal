/* Mike Rainey <mrainey@mpi-sws.org> */

#ifndef __MROPE_SPARSEMAT_OUTPUT_C__
#define __MROPE_SPARSEMAT_OUTPUT_C__

#include "mrope_sparsemat.c"

#include <stdio.h>

densevec_t densevec_output_core;
densevec_t densevec_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  densevec_fprint(file, densevec_output_core, sparsevec_size);
  //densevec_fprint(file, densevec_output_verf, sparsevec_size);
}

int cealtesthook_output_check() {
  return 1;
}

#endif
