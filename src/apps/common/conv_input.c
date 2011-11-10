/*Ezgi Cicek <ecicek@mpi-sws.org> */

#ifndef __CONV_INPUT_C__
#define __CONV_INPUT_C__

#include "scalar.c"
#include <stdio.h>
#include "conv.c"


static pixel_t* conv_input;
static long conv_input_size;
static int index;

void cealtesthook_input_generate(long size) {
  conv_input_size = size;
  long img_size = conv_input_size * conv_input_size;
  conv_input = malloc(sizeof(pixel_t) * img_size);

  /* Initialize the input image by setting all pixel values to 0
   * TODO: initialize the input image properly ( fill out
   * the pixels with right color values, ideally get parts
   * of a huge image
   */
  //memset(conv_input, 0, sizeof(pixel_t) * img_size);
  loadRawImage( "lena.raw", conv_input_size, conv_input_size, conv_input);
}

void cealtesthook_input_print(FILE* file) {
    fprintf( file, " ");
}


static pixel_t* conv_iter;
static long       change_size;
static pixel_t*     removed_cells;

void cealtesthook_input_iter_begin(long change_size_) {
  change_size = change_size_;
  conv_iter = &conv_input;
}

void cealtesthook_input_iter_next() {
    index++;

}

int cealtesthook_input_iter_isdone() {
  return (*conv_iter == NULL);
}

void cealtesthook_input_iter_change() {

  /* Save a pointer to removed cells. */
    removed_cells = malloc( sizeof(pixel_t)*change_size);

  /* Find the tail of the removed cells. */
  pixel_t* tl = conv_iter;
  int i = 0;
  
  while ( i < change_size && *tl != NULL ) {
    removed_cells[i] = conv_iter[i];
    conv_iter[i] = 0;
    i++;
  }
}

void cealtesthook_input_iter_revert() {

  /* Re-insert the removed cells. */
  int i = 0;
  while ( i < change_size && *conv_iter != NULL ) {
    conv_iter[i] = removed_cells[i];
    i++;
  }
  free(removed_cells);
}


#endif
