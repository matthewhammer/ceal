/*Ezgi Cicek <ecicek@mpi-sws.org> */

#ifndef __CONV_OUTPUT_C__
#define __CONV_OUTPUT_C__

#include <stdlib.h>
#include <stdio.h>
#include "conv_input.c"

pixel_t* conv_output_verf;
pixel_t* conv_output_core;

void cealtesthook_output_print(FILE* file, int i) {
    if(i == 0)
    {
        writeRawImage( CEAL_APP_SRC_PATH "/lena_conv_core.raw", conv_input_size, conv_input_size, conv_output_core);
    }
    else if (i == 1)
    {
        writeRawImage( CEAL_APP_SRC_PATH "/lena_conv_verf.raw", conv_input_size, conv_input_size, conv_output_verf);
    }
    else
        abort();
}

int cealtesthook_output_check() {
  long* conv_core = conv_output_core;
  long* conv_verf = conv_output_verf;
  int i = 0;
  while( i < conv_input_size*conv_input_size && conv_core && conv_verf) {

    if(! DATA_EQUAL(conv_core[i],conv_verf[i])) {
      fprintf(stderr, __FUNCTION__
              ": not equal: " DATA_FMT " <> " DATA_FMT "\n",
              DATA_FMT_ARGS(conv_core[i]),
              DATA_FMT_ARGS(conv_verf[i]));
      return 0;
    }
    else {
        i++;
        continue;
    }
  }

  int same_length =
    ( conv_core == NULL ) &&
    ( conv_verf == NULL ) ;

  if(! same_length ) {
    fprintf(stderr, __FUNCTION__ ": not equal: lengths are not equal, "
            "(%p == NULL && %p == NULL) does not hold.\n",
            conv_core[i], conv_verf[i]);
  }

  return same_length;
}


#endif
