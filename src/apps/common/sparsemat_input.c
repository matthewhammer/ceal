/* Matthew Hammer <hammer@uchicago.edu> */

#ifndef __SPARSEMAT_INPUT_C__
#define __SPARSEMAT_INPUT_C__

#include "sparsemat.c"

#include <stdio.h>

sparsemat_t sparsemat_input1;
sparsemat_t sparsemat_input2;
sparsevec_t sparsevec_input;
long        sparsevec_size;
sparsemat_t sparsemat_output;


typedef struct {
  index_t rowi;
  index_t coli;
  sparsevec_tl_t  new_vec;
  sparsevec_tl_t  old_vec;
  sparsevec_tl_t* vec_ptr;
} change_info_t;


static long           changec;
static change_info_t* changes;
static long           changes_rowi;

/*
static index_t* changed_rows;
static index_t* changed_cols;

static sparsevec_tl_t  new_vec;
static sparsevec_tl_t  old_vec;
static sparsevec_tl_t* vec_ptr;
*/

void cealtesthook_input_generate(long size) {
  sparsevec_size   = size;
  sparsevec_input  = sparsevec_random(size, 3.0 / size);
  sparsemat_input1 = sparsemat_random(size, 3.0 / size);
  sparsemat_input2 = sparsemat_random(size, 3.0 / size);
}

void cealtesthook_input_print(FILE* file) {

  fprintf(file, " [ ");
  sparsevec_fprint(file, sparsevec_input, sparsevec_size);
  fprintf(file, "]\n");
  
  fprintf(file, " [\n");
  sparsemat_fprint(file, sparsemat_input1);
  fprintf(file, "]\n");

  fprintf(file, " [\n");
  sparsemat_fprint(file, sparsemat_input2);
  fprintf(file, "]\n");
}

void cealtesthook_input_iter_begin(long change_size) {  
  changec = change_size;
  changes = malloc(sizeof(change_info_t) * changec);
  memset(changes, 0, sizeof(change_info_t) * changec);
  changes_rowi = 0;
}

void cealtesthook_input_iter_next() {
  changes_rowi += changec;
}

int cealtesthook_input_iter_isdone() {
  return !(changes_rowi < sparsemat_input1->rowc);
}

void cealtesthook_input_iter_change() {
  long i = 0;
  while ( i < changec
          &&
          changes_rowi + i < sparsemat_input1->rowc )
  {
    
    long coli = rand() % sparsemat_input1->colc;

    sparsevec_tl_t new_vec;
    sparsevec_tl_t old_vec;
    
    printf("changing row %ld, col %ld\n", changes_rowi + i, coli);
  
    /* Find the place for index=coli */
    sparsevec_tl_t* vec_ptr =
      sparsemat_find_ptr(sparsemat_input1, i + changes_rowi, coli);
    
    /* Is it occupied already? */
    if(*vec_ptr && (*vec_ptr)->index == coli) {
      printf("index %ld is already occupied\n", coli);
      
      /* It's already occupied => change its value. */
      old_vec = *vec_ptr;
      new_vec = alloc(struct sparsevec_s);
      new_vec->index = coli;
      new_vec->value = data_rand();
      new_vec->next  = old_vec->next;
      *vec_ptr = new_vec;
    }
    else {
      printf("index %ld is _not_ already occupied\n", coli);
      
      /* It's not already occupied => occupy it. */
      old_vec = NULL;
      new_vec = alloc(struct sparsevec_s);
      new_vec->index = coli;
      new_vec->value = data_rand();
      new_vec->next  = *vec_ptr;
      *vec_ptr = new_vec;
    }

    changes[i].rowi    = changes_rowi + i;
    changes[i].coli    = coli;
    changes[i].new_vec = new_vec;
    changes[i].old_vec = old_vec;
    changes[i].vec_ptr = vec_ptr;

    i++;
  }
}

void cealtesthook_input_iter_revert() {
  long i = 0;
  while ( i < changec
          &&
          changes_rowi + i < sparsemat_input1->rowc )
  {
    if(changes[i].old_vec) {
      *(changes[i].vec_ptr) = changes[i].old_vec;
    }
    else {
      *(changes[i].vec_ptr) = (changes[i].new_vec)->next;
    }
    i++;
  }  
}

#endif
