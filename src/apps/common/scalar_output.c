/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __SCALAR_OUTPUT_C__
#define __SCALAR_OUTPUT_C__

#include "scalar.c"
#include <stdio.h>

data_t owcr scalar_output_core;
data_t owcr scalar_output_verf;

void cealtesthook_output_print(FILE* file, int i) {
  if(i == 0)
    fprintf(file, DATA_FMT, DATA_FMT_ARGS(scalar_output_core));
  else if(i == 1)
    fprintf(file, DATA_FMT, DATA_FMT_ARGS(scalar_output_verf));
  else
    abort();
}

int cealtesthook_output_check() {    
  return DATA_EQUAL( scalar_output_core,
                     scalar_output_verf );
}
  
#endif
