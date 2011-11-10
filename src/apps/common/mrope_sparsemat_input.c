/* Mike Rainey <mrainey@mpi-sws.org> */

#ifndef __MROPE_SPARSEMAT_INPUT_C__
#define __MROPE_SPARSEMAT_INPUT_C__

#include "mrope_sparsemat.c"

#include <stdio.h>

static long            sparsevec_size;
static index_t         rowi;
static sparsemat_t     mtx;
static densevec_t      vec;
static sparsevec_t     svec;
static long            nrows;

void cealtesthook_input_generate(long size) {
  nrows = 100;
  sparsevec_size = size;
  mtx = sparsemat_random(nrows, size, 0.3);
  vec = densevec_random(size);
  svec = sparsevec_random(size, 0.3);
}

void cealtesthook_input_print(FILE* file) {
  fprintf(file, "mtx = \n");
  sparsemat_fprint(file, mtx);
  fprintf(file, "\n");
  fprintf(file, "vec = \n");
  densevec_fprint(file, vec, sparsevec_size);
  fprintf(file, "svec = \n");
  sparsevec_fprint(file, svec, sparsevec_size);
}

void cealtesthook_input_iter_begin() {
  rowi = 0;  
}

void cealtesthook_input_iter_next() {
  rowi++;
}

int cealtesthook_input_iter_isdone() {
  return !(rowi < sparsevec_size);
}

void cealtesthook_input_iter_change() {
  unsigned long i = rand() % mtx->nrows;
  unsigned long j = rand() % mtx->ncols;
  sparsemat_write_ib(mtx, i, j, data_rand());
}

void cealtesthook_input_iter_revert() {

}


#endif
