/* Ezgi Cicek <ecicek@mpi-sws.org>
 * Convolution with 1-1 correspondence
 * 1 pixel for each 1 modref
 */
#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "scalar.c"

#include "conv_input.c"
#include "conv_output.c"
#include "main.c"
#include "conv_1_1.c"


static void convolution(pixel_t* in, pixel_t* out)
{
    // define 5x5 Gaussian kernel
    long kernelRow = 5, kernelCol = 5;
    kernel_t kernel[25] = { 1/256.0f,  4/256.0f,  6/256.0f,  4/256.0f, 1/256.0f,
                         4/256.0f, 16/256.0f, 24/256.0f, 16/256.0f, 4/256.0f,
                         6/256.0f, 24/256.0f, 36/256.0f, 24/256.0f, 6/256.0f,
                         4/256.0f, 16/256.0f, 24/256.0f, 16/256.0f, 4/256.0f,
                         1/256.0f,  4/256.0f,  6/256.0f,  4/256.0f, 1/256.0f };
    printf("here");
    //convolve the image
    convolve2DSlow( in, out, conv_input_size, conv_input_size, kernel, kernelRow, kernelCol);
    /*TODO: write the colvolved image ( sofar, for the ceal stuff,
     *no need to write it out yet)*/
}

static void convolution_core(pixel_t* in, pixel_t* out){
    convolution( in, out);
}

void cealtesthook_run_core(){
    conv_output_core = malloc(sizeof(pixel_t)*conv_input_size * conv_input_size);
    core(convolution_core)(conv_input, conv_output_core);
}

void cealtesthook_run_verf(){
    convolution( conv_input, conv_output_core);
}
