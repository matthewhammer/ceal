/* Mike Rainey <mrainey@uchicago.edu> */

#ifndef __MROPE_SPARSEMAT_C__
#define __MROPE_SPARSEMAT_C__

#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "scalar.c"
#include "mrope.c"

#define NIL_INDEX (-1l)
#define SCALAR_ZERO 0l

/* Fixed fields that never change value: */
typedef long zwzr            index_t;
typedef data_t zwzr          value_t;

typedef struct sparsevec_val_s* sparsevec_val_t;

struct sparsevec_val_s {
  index_t index;
  value_t value;
};

/* due to lack of polymorphism in C, the type system fails to
   encapsulate the type of sparsevec_t: it actually has the type of an
   mrope whose data elements are of type sparsevec_val_t* */
/* we achieve log-time random access to individual sparse-vector
   elements by using binary search over the sparse-matrix indices
*/
/* the binary search relies on two invariants of our sparse-vector
   representation: the values of the sparse matrix appear in ascending
   order from left to right in the mrope that contains them and the
   mrope is such that each non-leaf mrope node caches the
   sparse-vector index of its rightmost non-empty leaf node */
typedef mrope_t sparsevec_t;

typedef struct sparsemat_s* sparsemat_t;

struct sparsemat_s {
  index_t      nrows;
  index_t      ncols;
  sparsevec_t* rows;
};

typedef data_t*         densevec_t;

static sparsevec_val_t
sparsevec_val_of_leaf(mrope_t m) {
  return((sparsevec_val_t)mrope_data(m));
}

static sparsevec_val_t
sparsevec_val(index_t index, data_t value) {
  sparsevec_val_t nv = alloc(struct sparsevec_val_s);
  nv->index = index;
  nv->value = value;
  return nv;
}

static sparsevec_t
sparsevec_singleton(index_t index, value_t value) {
  return(mrope_singleton(sparsevec_val(index, value)));
}

sparsevec_t
sparsevec_cat2(sparsevec_t l, sparsevec_t r) {
  sparsevec_t v = mrope_nbcat2(l, r);
  if (mrope_node(v) == CAT) {
    mrope_t l = mrope_left(v);
    mrope_t r = mrope_right(v);
    if (mrope_node(r) != EMPTY)
      v->node_u.cat_s.c = mrope_data(r);
    else if (mrope_node(l) != EMPTY)
      v->node_u.cat_s.c = mrope_data(l);
    else 
      v = mrope_empty();
  }
  return v;
}

void
sparsevec_finalize(sparsevec_t* p) {
  mrope_finalize(p);
  sparsevec_t v = *p;
  if (mrope_node(v) == CAT)
    if (mrope_node(mrope_right(v)) != EMPTY)
      v->node_u.cat_s.c = mrope_data(mrope_right(v));
    else if (mrope_node(mrope_left(v)) != EMPTY)
      v->node_u.cat_s.c = mrope_data(mrope_left(v));
    else
      abort();
}

/* allocate and initialize the sparse matrix structure excluding the
   rows */
sparsemat_t
sparsemat_skel(index_t nrows, index_t ncols) {
  sparsemat_t m = alloc(struct sparsemat_s);

  m->nrows = nrows;
  m->ncols = ncols;

  m->rows = malloc(sizeof(sparsevec_t) * nrows);
  memset(m->rows, 0, sizeof(sparsevec_t) * nrows);

  return m;
}

/* O(log n) computes the dot product of a sparse vector v and a dense
   vector w, both of length n */
void
sparsevec_densevec_dot(sparsevec_t v, densevec_t w, data_t z, data_t* d) {
  node_t t = mrope_node(v);
  if (t == EMPTY) {
    *d = z;
  } else if (t == LEAF) {
    sparsevec_val_t svv = sparsevec_val_of_leaf(v);
    *d = svv->value * w[svv->index];
  } else if (t == CAT) {
    data_t l;
    data_t r;
    sparsevec_densevec_dot(mrope_left(v), w, z, &l);
    sparsevec_densevec_dot(mrope_right(v), w, z, &r);
    *d = l + r;
  } else {
    abort();
  }
}

/* O(log l) multiplication of a sparse matrix by a dense vector */
/* we define l as the number of nonzeros in the longest row of the
   sparse matrix argument */
void
sparsemat_densevec_mult(sparsemat_t a, densevec_t v, data_t z, densevec_t d) {
  for (index_t i = 0; i < a->nrows; i++) /* loop in parallel */ 
    sparsevec_densevec_dot(a->rows[i], v, z, &(d[i]));
}

/* O(1) returns the largest index stored in the given sparse matrix */
index_t
sparsevec_largest_index(sparsevec_t v) {
  node_t t = mrope_node(v);
  if (t == EMPTY)
    return NIL_INDEX;
  else if (t == LEAF) {
    sparsevec_val_t sv = sparsevec_val_of_leaf(v);
    return(sv->index);
  } else if (t == CAT)
    return((index_t)mrope_data(v)); 
  else {
    abort();
    return 0;
  }
}

/* O(log n) split a sparse vector v with c columns and n nonzeros into
   two subvectors v[0, ..., n] and v[n + 1, ... c-1] */
/* it is required that 0 < n < c-1 */
void
sparsevec_split_sparseix_ib(sparsevec_t v, index_t n, mrope_pair_t* split) {
  node_t t = mrope_node(v);
  if (t == EMPTY) {
    split->l = mrope_empty();
    split->r = mrope_empty();
  } else if (t == LEAF) {
    sparsevec_val_t svv = sparsevec_val_of_leaf(v);
    if (svv->index <= n) {
      split->l = v;
      split->r = mrope_empty();
    } else {
      split->l = mrope_empty();
      split->r = v;
    }
  } else if (t == CAT) {
    index_t index_l = 
      sparsevec_largest_index(mrope_left(v));
    if (index_l == NIL_INDEX) {
      sparsevec_split_sparseix_ib(mrope_right(v), n, split);
    } else if (index_l == n) {
      split->l = mrope_left(v);
      split->r = mrope_right(v);
    } else if (index_l < n) {
      mrope_pair_t split_l;
      split->r = mrope_alloc();
      sparsevec_split_sparseix_ib(mrope_left(v), n, &split_l);
      split->l = split_l.l;
      split->r->node_u.cat_s.l = split_l.r;
      split->r->node_u.cat_s.r = mrope_right(v);
      sparsevec_finalize(&(split->r));
    } else {
      mrope_pair_t split_r;
      split->l = mrope_alloc();
      sparsevec_split_sparseix_ib(mrope_right(v), n, &split_r);
      split->l->node_u.cat_s.l = mrope_left(v);
      split->l->node_u.cat_s.r = split_r.l;
      sparsevec_finalize(&(split->l));
      split->r = split_r.r;
    }
  } else {
    abort();
  }
}

/* O(log n) splits the sparse vector argument into two subvectors l
   and r, such that l contains the first n vector elements and r
   contains the rest */
mrope_pair_t*
sparsevec_split2_ib(sparsevec_t m, index_t n) {
  assert(n > 0);
  assert(mrope_length(m) >= n);
  node_t t = mrope_node(m);
  mrope_pair_t split;
  if (t == EMPTY) {
    split.l = mrope_empty();
    split.r = mrope_empty();
  } else if (t == LEAF) {
    if (n == 0) {
      split.l = mrope_empty();
      split.r = m;
    } else {
      split.l = m;
      split.r = mrope_empty();
    }
  } else if (t == CAT) {
    long len = mrope_length(mrope_left(m));
    if (len == n) {
      split.l = mrope_left(m);
      split.r = mrope_right(m);
    } else if (n < len) {
      mrope_pair_t* split_l =
	sparsevec_split2_ib(mrope_left(m), n);
       split.l = split_l->l;
      split.r = sparsevec_cat2(split_l->r, mrope_right(m));
    } else {
      mrope_pair_t* split_r = 
	sparsevec_split2_ib(mrope_right(m), n - len);
      split.l = sparsevec_cat2(mrope_left(m), split_r->l);
      split.r = split_r->r;
    }
  } else {
    abort();
  }
  return &split;
}

/* O(log n) builds a perfectly-balanced copy of the argument sparsevec m */
/* this algorithm is unstable */
sparsevec_t
sparsevec_perf_bal(sparsevec_t m) {
  long len = mrope_length(m);
  if (len < 2) {
    return(mrope_shallow_compact(m));
  } else if (is_balanced(m)) {
    return m;
  } else {
    mrope_pair_t* split = sparsevec_split2_ib(m, len / 2);
    mrope_t l = sparsevec_perf_bal(split->l);
    mrope_t r = sparsevec_perf_bal(split->r);
    return(sparsevec_cat2(l, r));
  }
}

/* O(log m) write nonzero value x into v[i], where sparse vector v has
   m nonzeros */
/* we require that 0 < i < c-1, where c is the length of v */
static void
sparsevec_write_ib(sparsevec_t* p, index_t i, data_t x) {
  mrope_t m = *p;
  node_t t = mrope_node(m);
  if (t == EMPTY) {
    *p = sparsevec_singleton(i, x);
  } else if (t == LEAF) {
    sparsevec_val_t sv = sparsevec_val_of_leaf(m);
    if (sv->index == i)
      sv->value = x;
    else if (sv->index < i)
      *p = sparsevec_cat2(m, sparsevec_singleton(i, x));
    else
      *p = sparsevec_cat2(sparsevec_singleton(i, x), m);
  } else if (t == CAT) {
    index_t index_l =
      sparsevec_largest_index(mrope_left(m));
    if (index_l == NIL_INDEX)
      sparsevec_write_ib(mrope_addr_right(m), i, x);
    else if (i <= index_l)
      sparsevec_write_ib(mrope_addr_left(m), i, x);
    else
      sparsevec_write_ib(mrope_addr_right(m), i, x);
    sparsevec_finalize(p);
    if (! mrope_is_cbalanced(*p))
      *p = sparsevec_perf_bal(*p);
  } else {
    abort();
  }
}

void
sparsemat_write_ib(sparsemat_t mtx, index_t i, index_t j, data_t x) {
  assert(i >= 0 && i < mtx->nrows);
  assert(j >= 0 && j < mtx->ncols);
  sparsevec_write_ib(&(mtx->rows[i]), j, x);
}

static mrope_t*
sparsevec_sub_ib_addr(sparsevec_t* p, index_t i) {
  mrope_t m = *p;
  node_t t = mrope_node(m);
  if (t == CAT) {
    index_t index_l = 
      sparsevec_largest_index(mrope_left(m));
    if (index_l == NIL_INDEX)
      return(sparsevec_sub_ib_addr(mrope_addr_right(m), i));
    else if (i <= index_l)
      return(sparsevec_sub_ib_addr(mrope_addr_left(m), i));
    else
      return(sparsevec_sub_ib_addr(mrope_addr_right(m), i));
  } else /* if (t == LEAF || t == EMPTY) */ {
    return p;
  }
}

/* O(log n) returns the value at position i in sparse vector v (or 0
   if the value at position i is zero) */
/* index i is assumed to satisfy that 0 < i < c-1, where c and n are
   the the number of columns in v and the number of nonzeros
   respectively */
sparsevec_val_t
sparsevec_sub_ib(sparsevec_t v, index_t i) {
  sparsevec_t x = v;
  mrope_t* p = sparsevec_sub_ib_addr(&x, i);
  if (mrope_is_empty(*p))
    return 0;
  else if (sparsevec_val_of_leaf(*p)->index == i)
    return(sparsevec_val_of_leaf(*p));
  else
    return 0;
}

/* O(log n) returns two sparse vectors v[0, ..., p-1] and v[p, ...,
   length(v)-1] where 0 < p < length(v)-1 is chosen at random */
/* random split point p is chosen by creating a cut based on a
   randomly chosen path from the root to a leaf of the mrope that
   represents v */
void
sparsevec_rand_split2(sparsevec_t v, mrope_pair_t *split) {
  node_t t = mrope_node(v);
  if (t == EMPTY) {
    split->l = mrope_empty();
    split->r = mrope_empty();
  } else if (t == LEAF) {
    if (rand() % 2 == 0) {
      split->l = mrope_singleton(sparsevec_val_of_leaf(v));
      split->r = mrope_empty();
    } else {
      split->l = mrope_empty();
      split->r = mrope_singleton(sparsevec_val_of_leaf(v));
    }
  } else if (t == CAT) {
    if (rand() % 2 == 0) {
      mrope_pair_t cl;
      sparsevec_rand_split2(mrope_left(v), &cl);
      split->l = cl.l;
      split->r = sparsevec_cat2(cl.r, mrope_right(v));
    } else {
      mrope_pair_t cr;
      sparsevec_rand_split2(mrope_right(v), &cr);
      split->l = sparsevec_cat2(mrope_left(v), cr.l);
      split->r = cr.r;
    }
  } else {
    abort();
  }
}

/* O(log l) computes the dot product of two sparse vectors v and u */
/* l denotes the maximum number of nonzeros contained in either v or u */
void
sparsevec_sparsevec_dot(sparsevec_t v, sparsevec_t u, data_t z, data_t* d) {
  node_t t = mrope_node(v);
  if (t == EMPTY) {
    *d = z;
  } else if (t == LEAF) {
    sparsevec_val_t svv = sparsevec_val_of_leaf(v);
    sparsevec_val_t svu = sparsevec_sub_ib(u, svv->index);
    *d = svv->value * (svu ? svu->value : z);
  } else if (t == CAT) {
    mrope_pair_t split_v;
    mrope_pair_t split_u;
    data_t l;
    data_t r;
    sparsevec_rand_split2(v, &split_v);
    index_t index_l =
      sparsevec_largest_index(split_v.l);
    if (index_l == NIL_INDEX)
      sparsevec_sparsevec_dot(split_v.r, u, z, d);
    else {
      sparsevec_split_sparseix_ib(u, index_l, &split_u);
      sparsevec_sparsevec_dot(split_v.l, split_u.l, z, &l);
      sparsevec_sparsevec_dot(split_v.r, split_u.r, z, &r);
      *d = l + r;
    }
  } else {
    abort();
  }
}

/* O(log l) multiplies sparse matrix mtx by sparsevector v */
/* l denotes the maximum number of nonzeros contained in any of the
   rows of mtx or in v */
void
sparsemat_sparsevec_mult(sparsemat_t mtx, sparsevec_t v, data_t z, densevec_t d) {
  for (index_t i = 0; i < mtx->nrows; i++) /* loop in parallel */
    sparsevec_sparsevec_dot(mtx->rows[i], v, z, &(d[i]));
}

void
densevec_fprint(FILE* file, densevec_t d, long size) {
  fprintf(file, "[");
  for (long i = 0; i < size; i++)
    fprintf(file, "" DATA_FMT " ", d[i]);
  fprintf(file, "]");
}

void generate_long_array(long size, double p);
long long_array_subi(long i);
long long_array_size();

static index_t
interval_length(index_t lo, index_t hi) {
  return(hi - lo + 1);
}

static sparsevec_t
sparsevec_random_rec(index_t lo, index_t hi) {
  index_t len = interval_length(lo, hi);
  if (len == 0) {
    return(mrope_empty());
  } else if (len == 1) {
    info_t index = long_array_subi(lo);
    return(sparsevec_singleton(index, data_rand() % 10));
  } else {
    index_t m = interval_length(lo, hi) / 2 + lo;
    sparsevec_t l = sparsevec_random_rec(lo, m - 1);
    sparsevec_t r = sparsevec_random_rec(m,  hi);
    return(sparsevec_cat2(l, r));
  }
}

static sparsevec_t
sparsevec_random(long size, double p) {
  generate_long_array(size, p);
  return(sparsevec_random_rec(0, long_array_size() - 1));
}

static sparsemat_t
sparsemat_random(long nrows, long ncols, double density) {
  sparsemat_t mtx = sparsemat_skel(nrows, ncols);
  for (long i = 0; i < nrows; i++)
    mtx->rows[i] = sparsevec_random(ncols, density);
  return mtx;
}

static densevec_t
densevec_random(long size) {
  densevec_t dv = malloc(sizeof(densevec_t) * size);
  memset(dv, 0, sizeof(data_t) * size);
  for (index_t i = 0; i < size; i++)
    dv[i] = data_rand() % 10;
  return dv;
}

void
sparsevec_fprint(FILE* file, 
		 sparsevec_t v, 
		 long size) {
  fprintf(file, "[ ");
  for (long i = 0; i < mrope_length(v); i++) {
    sparsevec_val_t sv = sparsevec_val_of_leaf(mrope_sub_ib_node(v, i));
    fprintf(file, "(%d, %d, %d) ", i, sv->index, sv->value);
  }
  fprintf(file, " ]");
  fprintf(file, "[ ");
  for (long i = 0; i < size; i++) {
    sparsevec_val_t sv = sparsevec_sub_ib(v, i);
    if (sv != 0)
      fprintf(file, "" DATA_FMT " ", sv->value);
    else
      fprintf(file, "" DATA_FMT " ", SCALAR_ZERO);
  }

  fprintf(file, " ]");
}

void
sparsemat_fprint(FILE* file, sparsemat_t m) {
  for (long i = 0; i < m->nrows; i++) {
    sparsevec_fprint(file, m->rows[i], m->ncols);
    fprintf(file, "\n");
  }
}

#endif
