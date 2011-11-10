/* Matthew Hammer <hammer@uchicago.edu> */

#ifndef __SPARSEMAT_C__
#define __SPARSEMAT_C__

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "scalar.c"

/* Sparse vectors -- linked lists of index-value pairs. */
typedef struct sparsevec_s* sparsevec_t;

#define SPARSEMAT_OWCR  0
#define SPARSEMAT_SHARE 1

#if SPARSEMAT_OWCR
typedef sparsevec_t owcr sparsevec_tl_t ;
#else
typedef sparsevec_t owcr sparsevec_tl_t ;
#endif

/* Fixed fields that never change value: */
typedef long zwzr            index_t;
typedef data_t zwzr          value_t;
typedef sparsevec_tl_t* zwzr rowv_t;

struct sparsevec_s {
  index_t        index;
  value_t        value;
  sparsevec_tl_t next;
};

/* Sparce matricies -- an array of sparse vectors. */
typedef struct sparsemat_s* sparsemat_t;

struct sparsemat_s {
  index_t rowc;
  index_t colc;
  rowv_t  rowv;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Sparse vector operations */

static void sparsevec_fprint(FILE* file, sparsevec_t v, long size) {

#if 0
  while( v ) {
    fprintf(file, "(%ld," DATA_FMT "), ", v->index, v->value);
    v = v->next;
  }
  
#else
  for(long i = 0; i < size; i++) {
    if(v == NULL) {
      fprintf(file, "======== ");
      /*fprintf(file, "== ");*/
    }
    else if(i < v->index) {
      fprintf(file, "-------- ");
      /*fprintf(file, "-- ");*/
    }
    else {
      if(v->index < i) {
        abort();
      }
      else if(v->index == i)
      {
        fprintf(file, "" DATA_FMT " ", v->value);
        v = v->next;
      }
    }
  }
#endif
  
}

static sparsevec_t sparsevec_cons(index_t index, value_t value) {
  sparsevec_t v = alloc(struct sparsevec_s);
  v->index = index;
  v->value = value;
  return v;
}

void random_sorted_longs_generate(long num, long range);
long random_sorted_longs_ith(long i);

static void sparsevec_random_rec(long idx_i,
                                 long idx_last,
                                 long idx_c,
                                 sparsevec_tl_t* d) {

  if(idx_i == idx_c) {
    *d = NULL;
  }
  else {
    long index = random_sorted_longs_ith(idx_i);

    if(index > idx_last) {
      /* This index is distinct from the last one. */
      sparsevec_t v = sparsevec_cons(index, data_rand());
      *d = v;
      sparsevec_random_rec(idx_i + 1, index, idx_c, &v->next);
    }
    else {
      /* This index is not unique: the last one is identical.
         So, continue onward without creating a cons cell. */
      sparsevec_random_rec(idx_i + 1, index, idx_c, d);
    }
  }
}


static sparsevec_t sparsevec_random(long size, double density) {
  sparsevec_tl_t v;  
  long num_occupants = (long) (density * size);
  random_sorted_longs_generate(num_occupants, size);
  sparsevec_random_rec(0, -1, num_occupants, &v);
  return v;  
}

static long sparsevec_equal(sparsevec_tl_t v1, sparsevec_tl_t v2) {  
  while(v1 && v2) {
    if(v1->index == v2->index &&
       v2->value == v2->value) {    
      v1 = v1->next, v2 = v2->next;
      continue;
    }
    else
      return 0;
  }
  /* Post condition: at least one is NULL. */
  /* Are they both NULL? */
  return v1 == v2;
}

/* sparsevec_find_ptr:
 * Want the (location of) the pointer to the given index.
 */
static sparsevec_tl_t* sparsevec_find_ptr(sparsevec_tl_t* vp, index_t index) {
  if((*vp) == NULL ||
     index <= (*vp)->index) {
    return vp;
  }
  else {
    return sparsevec_find_ptr(&((*vp)->next), index);
  }
}

static void sparsevec_reverse_rec(sparsevec_t v,
                                  sparsevec_t reversed,
                                  sparsevec_tl_t* d) {
  if(v == NULL) {
    *d = reversed;
  }
  else {
    sparsevec_t w = memo(sparsevec_cons(v->index, v->value));
    w->next = reversed;
    memo;
    sparsevec_reverse_rec(v->next, w, d);
  }
}
  
static sparsevec_tl_t sparsevec_reverse(sparsevec_t v) {
  sparsevec_tl_t d;
  sparsevec_reverse_rec(v, NULL, &d);
  return d;
}

static data_t sparsevec_dot(sparsevec_t v, sparsevec_t w) {
  data_t sum = 0;
  
  while(v && w) {
    long v_index = v->index;
    long w_index = w->index;
    
    if(v_index == w_index) {
      sum += v->value * w->value;
      v = v->next;
      w = w->next;
    }
    else if(v_index < w_index) {
      v = v->next;
    }
    else if(w_index < v_index) {
      w = w->next;
    }
    else {
      abort();
    }
  }
  
  return sum;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Sparse matrix operations */

static void sparsemat_fprint(FILE* file, sparsemat_t m) {
  for(long i = 0; i < m->rowc; i++) {
    fprintf(file, "row %ld: ", i);
    sparsevec_fprint(file, m->rowv[i], m->colc);
    fprintf(file, "\n");
  }
}

static sparsevec_t* sparsemat_awar_rowv(long rowc) {
  sparsevec_t* rowv = malloc(sizeof(sparsevec_t) * rowc);
  memset(rowv, 0, sizeof(sparsevec_t) * rowc);
  return rowv;
}

/* Allocate & intialize a sparsemat structure.
   But, does not initialize the rows. */
static sparsemat_t sparsemat_skel(long rowc, long colc) {
  sparsemat_t m = alloc(struct sparsemat_s);

  m->rowc = rowc;
  m->colc = colc;

#define FIXED 0
#if FIXED
  /* TODO: Parsing support for non-constant-sized arrays */
  m->rowv = alloc(sparsevec_t[size]);
  for(long i = 0; i < rowc; i++) {
    m->rowv[i] = 0;
  }
#else
  m->rowv = malloc(sizeof(sparsevec_t) * rowc);
  memset(m->rowv, 0, sizeof(sparsevec_t) * rowc);
#endif     
  
  return m;
}

static sparsemat_t sparsemat_random(long size, double density) {

  sparsemat_t m = sparsemat_skel(size, size);
  
  for(long i = 0; i < size; i++) {
    m->rowv[i] = sparsevec_random(size, density);
  }

  return m;
}

static sparsevec_t* sparsemat_find_ptr(sparsemat_t m, long rowi, long coli) {
  return sparsevec_find_ptr(&(m->rowv[rowi]), coli);
}

/* Test if two matricies are equal. */
static long sparsemat_equal(sparsemat_t m1, sparsemat_t m2) {
  if ( m1->rowc == m2->rowc &&
       m2->colc == m2->colc ) {
    for(long i = 0; i < m1->rowc; i++) {
      if(sparsevec_equal(m1->rowv[i], m2->rowv[i]))
        continue;
      else
        return 0;
    }
    return 1;
  }
  else return 0;           
}

/* Transpose a sparse matrix. */
static sparsemat_t sparsemat_transpose(sparsemat_t m_in) {

  sparsemat_t m_out = sparsemat_skel(m_in->colc, m_in->rowc);
  sparsevec_t* rowv = sparsemat_awar_rowv(m_in->colc);

  /* Initialize each row in rowv to be empty. */
  for(long i = 0; i < m_in->colc; i++) {
    rowv[i] = 0;
  }
  
  /* For each row i in input, copy cell at col j to (row=j, col=i) in
     output, but in reverse order (this is easiest without resorting
     to pointing to pointers. */
  for(long i = 0; i < m_in->rowc; i++) {
    cut {
      sparsevec_t v_in = m_in->rowv[i];
      
      while(v_in) {
        data_t value = v_in->value;
        
        sparsevec_t w =
          memo(sparsevec_cons(i, value));
        memo;
        
        long j = v_in->index;

        cut {
          w->next = rowv[j];
          rowv[j] = w;
        }

        /* Advance to the next input cell. */
        v_in = v_in->next;
      }
    }
  }
  
  /* (Un)reverse each new row. */
  for(long j = 0; j < m_out->rowc; j++) {
    cut {
      m_out->rowv[j] = sparsevec_reverse(rowv[j]);
    }
  }
  
  return m_out;
}

/* Multiply a sparse matrix by a sparse vector. */
static sparsevec_t sparsemat_vec_mult(sparsemat_t a, sparsevec_t v) {
  sparsevec_t  null = NULL;
  sparsevec_t* rev  = &null;
  
  for(long i = 0; i < a->rowc; i++) {
    cut {
      value_t dot = sparsevec_dot(a->rowv[i], v);
      
      if(! DATA_EQUAL(dot, 0)) {
        sparsevec_t w = memo(sparsevec_cons(i, dot));
        w->next = *rev;
        *rev = w;        
      }
    }
  }

  return sparsevec_reverse(*rev);
}

/* Multiply a sparse matrix by another sparse matrix. */
static sparsemat_t sparsemat_mat_mult(sparsemat_t a, sparsemat_t b) {
  
  assert(a->colc == b->rowc &&
         "Matrix multiplication defined when "
         "colc of 1st matches rowc of 2nd");

  sparsemat_t c = sparsemat_transpose(b);
  sparsemat_t d = sparsemat_skel(c->rowc, c->colc);

  for(long i = 0; i < b->colc; i++) {
    cut {
      /* Note: the read of rowv[i] may awaken. */
      d->rowv[i] = sparsemat_vec_mult(a, c->rowv[i]);
    }
  }

  return sparsemat_transpose(d);
}

#endif
