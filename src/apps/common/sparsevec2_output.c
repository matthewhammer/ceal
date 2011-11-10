/* Matthew Hammer <hammer@uchicago.edu> */

#ifndef __SPARSEVEC2_OUTPUT_C__
#define __SPARSEVEC2_OUTPUT_C__

#include "sparsemat2.c"

#include <stdio.h>

long        sparsevec_output_size = 0;
sparsemat_t sparsevec_output_core;
sparsemat_t sparsevec_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  fprintf(file, " [ ");
  if(i == 0)    
    sparsevec_fprint(file,
                     sparsevec_output_core,
                     sparsevec_output_size);
  else if(i == 1)
    sparsevec_fprint(file,
                     sparsevec_output_verf,
                     sparsevec_output_size);
  else
    abort();
  fprintf(file, " ]\n");
}

int cealtesthook_output_check() {
  return sparsevec_equal(sparsevec_output_core,
                         sparsevec_output_verf);
}

#endif
