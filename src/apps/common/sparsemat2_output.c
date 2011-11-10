/* Matthew Hammer <hammer@uchicago.edu> */

#ifndef __SPARSEMAT2_OUTPUT_C__
#define __SPARSEMAT2_OUTPUT_C__

#include "sparsemat2.c"

#include <stdio.h>

sparsemat_t sparsemat_output_core;
sparsemat_t sparsemat_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  fprintf(file, " [\n");
  if(i == 0)    
    sparsemat_fprint(file, sparsemat_output_core);
  else if(i == 1)
    sparsemat_fprint(file, sparsemat_output_verf);
  else
    abort();
  fprintf(file, "]\n");
}

int cealtesthook_output_check() {
  return sparsemat_equal(sparsemat_output_core,
                         sparsemat_output_verf);
}

#endif
