/* Matthew Hammer <hammer@tti-c.edu> */

#ifndef __ARRTREE_C__
#define __ARRTREE_C__

#include <stdio.h>
#include <assert.h>
#include "scalar.c"

typedef struct arrnode_s* arrtree_t;

struct arrnode_s {
  data_t*   arr;
  long      len;
  arrtree_t left;
  arrtree_t right;
};

static arrtree_t arrtree_node(data_t* arr, long len) {
  arrtree_t t = alloc(struct arrnode_s);
  t->arr   = arr;
  t->len   = len;
  t->left  = NULL;
  t->right = NULL;
  return t;
}

#define ARRTREE_ARRAY_LEN 3

static data_t* arrtree_random_array() {
  data_t* arr = alloc(data_t[ARRTREE_ARRAY_LEN]);
  for(long i = 0; i < ARRTREE_ARRAY_LEN; i++) {
    arr[i] = data_rand();      
  }
  return arr;
}

static arrtree_t arrtree_random_balanced(long size) {
  if(size > 0) {

    long size_right = (size - 1) / 2;
    long size_left  = (size - 1) - size_right;
    
    arrtree_t t = arrtree_node(arrtree_random_array(),
                               ARRTREE_ARRAY_LEN);
    
    t->left  = arrtree_random_balanced(size_left);
    t->right = arrtree_random_balanced(size_right);
    
    return t;
  }
  else {
    return NULL;
  }
}

static long arrtree_ptrs_preorder(arrtree_t t, arrtree_t** ptrs, long pos) {
  if(t) {
    ptrs[pos + 0] = &(t->left);
    ptrs[pos + 1] = &(t->right);
    
    pos = arrtree_ptrs_preorder(t->left,  ptrs, pos + 2);    
    pos = arrtree_ptrs_preorder(t->right, ptrs, pos);
  }
  return pos;
}

static void arrtree_fprint_rec(FILE* file, arrtree_t t, long indent) {
  if(t == NULL)
    fprintf(file, "nil");
  else {
    fprintf(file, "(node [");
    for(long i = 0; i < t->len; i++) {
      fprintf(file, DATA_FMT "%s",
              t->arr[i],
              (i + 1 < t->len
               ? ", "
               : "")
              );
    }
    fprintf(file, "] \n");

    for(int i = 0; i < indent; i++)
      fprintf(file, "  ");
    
    arrtree_fprint_rec(file, t->left, indent+1);
    fprintf(file, " \n");

    for(int i = 0; i < indent; i++)
      fprintf(file, "  ");

    
    arrtree_fprint_rec(file, t->right, indent+1);
    fprintf(file, ")");
  }
}

static void arrtree_fprint(FILE* file, arrtree_t t) {
  arrtree_fprint_rec(file, t, 0);
}

#endif
