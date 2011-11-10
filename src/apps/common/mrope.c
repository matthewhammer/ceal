/* Mike Rainey <mrainey@mpi-sws.org>   */
/* Matthew Hammer <hammer@mpi-sws.org> */

#ifndef __MROPE_C__
#define __MROPE_C__

#include <stdio.h>
#include <assert.h>

/* mropes

  The mrope is a data structure for representing large sequences of
  data elements. It admits efficient parallel operations, such as map,
  reduce, and filter, as well as efficient insertions and deletions of
  individual data elements. Our mropes are balanced binary trees with
  single data elements at their leaves.

    Mrope ::= Empty
            | Leaf Data
	    | Cat Int Int Mrope Mrope

  Read from left to right, the data elements at the leaves of a mrope
  constitute the data of the sequence it represents. In the Leaf node
  Leaf d, the data element d denotes the data element associated with
  the leaf. The internal mrope nodes, or Cat nodes, cache length and
  depth information. We use these fields to maintain mrope balance
  across sequences of insertions and deletions. In the Cat node
  Cat(len, dep, l, r), the integer len denotes the number of data
  elements under the Cat node, the integer dep denotes the depth of
  the submrope rooted at the Cat node, and the mropes l and r denote
  the left and right branches respectively. We define functions

    length m

  which returns the length field of m and

    depth m

  which returns the depth field of m.

  We maintain the property that every mrope is balanced within a
  constant factor of perfect balance. That is, for a given mrope m,

    depth m < alpha * log2 (length m)

  where alpha > 1 is a constant factor.

  We now describe the policy for inserting individual data
  elements. The operation

    insert (m, i, d)

  inserts data element d before the ith data element of mrope m. This
  operation begins by walking down to the leaf node Leaf d' that
  contains the ith data element and then stops and replaces it with
  Cat(2, 1, Leaf d, Leaf d'). On the way back up, the cached length and
  depth information is updated.

  The operation

    delete (m, i)

  deletes the ith data element of m by walking down mrope m to the ith
  Leaf node and replacing the node with an Empty node. On the way back
  up, the cached length and depth information is updated.

  Our approach to balancing is similar to that of scapegoat trees.

 */

typedef struct node_s* rope_t;
typedef rope_t mrope_t;
typedef long unsigned info_t;
typedef long bool;

typedef enum {
  JUNK,
  EMPTY,
  LEAF,
  CAT
} node_h_t;

typedef long unsigned node_t;

struct node_s {
  node_t n;
  union {
    struct {
      data_t  d;
    } leaf_s;
    struct {
      data_t  c;   // monoid-cached data
      info_t  len;
      info_t  dep;
      mrope_t l;
      mrope_t r;
    } cat_s;
  } node_u;
};

static long max_long(long a, long b) {
  return (a > b) ? a : b;
}

/* O(1) empty mrope */
mrope_t
mrope_empty () {
  return 0;
}

/* O(1) mrope empty check */
bool
mrope_is_empty(mrope_t m) {
  return(m == mrope_empty());
}

/* O(1) returns EMPTY if mrp is the empty rope, LEAF if mrp is a leaf
   mrope, and CAT if m is a cat mrope */
node_t
mrope_node(mrope_t m) {
  if (m == 0)
    return EMPTY;
  else
    return (m->n);
}
/* O(1) left branch of m */
mrope_t
mrope_left(mrope_t m) {
  return m->node_u.cat_s.l;
}
/* O(1) right branch of m */
mrope_t
mrope_right(mrope_t m) {
  return m->node_u.cat_s.r;
}
/* O(1) data element of a given nonempty mrope node */
data_t
mrope_data(mrope_t m) {
  node_t t = mrope_node(m);
  if (t == LEAF) {
    return(m->node_u.leaf_s.d);
  } else if (t == CAT) {
    return(m->node_u.cat_s.c);
  } else {
    abort();
    return(m->node_u.cat_s.c);
  }
}
/* O(1) length of mrope m */
long
mrope_length(mrope_t m) {
  node_t t = mrope_node(m);
  if (t == EMPTY)
    return 0;
  else if (t == LEAF)
    return 1;
  else if (t == CAT)
    return(m->node_u.cat_s.len);
  else {
    abort();
    return 0;
  }
}
/* O(1) depth of mrope m */
long
mrope_depth(mrope_t m) {
  node_t t = mrope_node(m);
  if (t == EMPTY)
    return 0;
  else if (t == LEAF)
    return 0;
  else if (t == CAT)
    return(m->node_u.cat_s.dep);
  else {
    abort();
    return 0;
  }
}
/* O(1) singleton rope for data element d */
mrope_t
mrope_singleton(data_t d) {
  mrope_t m = alloc(struct node_s);
  m->n = LEAF;
  m->node_u.leaf_s.d = d;
  return m;
}
/* O(1) concatenate two mropes */
/* empty subropes are eliminated; i.e., mrope_nbcat2(Empty, r) = r and
   mrope_nbcat2(l, Empty) = l */
/* no attempt is made to balance the mrope of the result */
mrope_t
mrope_nbcat2(mrope_t l, mrope_t r) {
  if (mrope_is_empty(l)) {
    return r;
  } else if (mrope_is_empty(r)) {
    return l;
  } else {
    mrope_t m = alloc(struct node_s);
    m->n = CAT;
    m->node_u.cat_s.len = 
      mrope_length(l) + mrope_length(r);
    m->node_u.cat_s.dep = 
      max_long(mrope_depth(l), mrope_depth(r)) + 1;
    m->node_u.cat_s.l = l;
    m->node_u.cat_s.r = r;
    return m;
  }
}

/* O(1) allocated (but do not initialize) an mrope */
mrope_t
mrope_alloc() {
  mrope_t m = alloc(struct node_s);
  return m;
}

/* O(1) address of the left branch of the argument mrope */
mrope_t*
mrope_addr_left(mrope_t m) {
  return(&(m->node_u.cat_s.l));
}

/* O(1) address of the right branch of the argument mrope */
mrope_t*
mrope_addr_right(mrope_t m) {
  return(&(m->node_u.cat_s.r));
}

/* O(1) update the node information of the argument rope */
/* we assume that just the left and right branches of the argument
   rope are populated */
void
mrope_finalize(mrope_t *d) {
  mrope_t m = *d;
  mrope_t l = mrope_left(m);
  mrope_t r = mrope_right(m);
  if (mrope_is_empty(l)) {
    *d = r;
  } else if (mrope_is_empty(r)) {
    *d = l;
  } else {
    // if (m-> != CAT) (uncommenting this line should expose a runtime error)
    m->n = CAT;
    m->node_u.cat_s.len = 
      mrope_length(l) + mrope_length(r);
    m->node_u.cat_s.dep = 
      max_long(mrope_depth(l), mrope_depth(r)) + 1;
  }
}

typedef struct {
  mrope_t l ;
  mrope_t r ;
} mrope_pair_t;

/* O(1) eliminates any empty subrope in the right or left branch of
   the argument mrope m  */
mrope_t
mrope_shallow_compact(mrope_t m) {
  node_t t = mrope_node(m);
  /* note that uncommenting the commented code above causes ceal
     compilation to fail */
  /* if (t == EMPTY || t == LEAF) { */
  if (t == EMPTY) { 
    return m;
  } else if (t == LEAF) {
    return(mrope_singleton(mrope_data(m)));
  } else if (t == CAT) {
    if (mrope_length(mrope_left(m)) == 0)
      return(mrope_right(m));
    else if (mrope_length(mrope_right(m)) == 0)
      return(mrope_left(m));
    else
      return m;
  } else {
    return m;
  }
}

/* O(log n) splits mrope m into two submropes called l and r, such
   that l contains the first n data elements of rp and r contains the
   rest */
/* no attempt is made to balance either of the result mropes */
mrope_pair_t*
mrope_nbsplit2(mrope_t m, long n) {
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
	mrope_nbsplit2(mrope_left(m), n);
       split.l = split_l->l;
      split.r = mrope_nbcat2(split_l->r, mrope_right(m));
    } else {
      mrope_pair_t* split_r = 
	mrope_nbsplit2(mrope_right(m), n - len);
      split.l = mrope_nbcat2(mrope_left(m), split_r->l);
      split.r = split_r->r;
    }
  } else {
    abort();
  }
  return &split;
}

/* O(log n) returns the node that contains the ith data element of
   mrope m, counting from 0 */
/* index i is assumed to be in bounds, i.e., 0 <= i < length m */
mrope_t
mrope_sub_ib_node(mrope_t m, long i) {
  node_t t = mrope_node(m);
  if (t == LEAF) {
    return m;
  } else if (t == CAT) {
    if (i < mrope_length(mrope_left(m)))
      return(mrope_sub_ib_node(mrope_left(m), i));
    else
      return(mrope_sub_ib_node
	     (mrope_right(m),
	      i - mrope_length(mrope_left(m))));
  } else {
    abort();
    return 0;
  }
}

/* O(log n) returns the address of the Cat-node branch that contains
   the ith data element of mrope m, counting from 0 */
/* index i is assumed to be in bounds, i.e., 0 <= i < length m */
mrope_t*
mrope_sub_ib_addr_node(mrope_t m, long i) {
  node_t t = mrope_node(m);
  if (t == CAT) {
    if ((mrope_node(mrope_left(m)) == LEAF) && (i == 0)) {
      return(mrope_addr_left(m));
    } else if ((mrope_node(mrope_right(m)) == LEAF) && 
	       (i == mrope_length(mrope_left(m)))) {
      return(mrope_addr_right(m));
    } else if (i < mrope_length(mrope_left(m))) {
      return(mrope_sub_ib_addr_node(mrope_left(m), i));
    } else {
      return(mrope_sub_ib_addr_node
	     (mrope_right(m),
	      i - mrope_length(mrope_left(m))));
    }
  } else {
    return 0;
  }
}

unsigned long
floor_lg (unsigned long v) {
  return(64l - __builtin_clzl(v) - 1l);
}

unsigned long
ceil_lg (unsigned long v) {
  unsigned long lg = floor_lg(v);
  return(lg + (v - (1l<<lg) > 0l));
}

#define C 2l

bool
is_cbalanced(info_t dep, info_t len) {
  return(dep <= C * ceil_lg(len));
}

bool
mrope_is_cbalanced(mrope_t m) {
  return(is_cbalanced(mrope_depth(m), mrope_length(m)));
}

bool
is_balanced(mrope_t m) {
  return(mrope_depth(m) <= ceil_lg(mrope_length(m)));
}

/* O(log n) builds a perfectly-balanced copy of the argument mrope m */
/* this algorithm is unstable */
mrope_t
mrope_perf_bal(mrope_t m) {
  long len = mrope_length(m);
  if (len < 2) {
    return(mrope_shallow_compact(m));
  } else if (is_balanced(m)) {
    return m;
  } else {
    mrope_pair_t* split = mrope_nbsplit2(m, len / 2);
    mrope_t l = mrope_perf_bal(split->l);
    mrope_t r = mrope_perf_bal(split->r);
    return(mrope_nbcat2(l, r));
  }
}

/* O(log n) ensures that the mrope pointed to by p is sufficiently
   balanced */
info_t
mrope_cbal_bu(mrope_t* p) {
  mrope_t m = *p;
  node_t t = mrope_node(m);
  if (t == EMPTY || t == LEAF) {
    return 0;
  } else if (t == CAT) {
    info_t dl = mrope_cbal_bu(mrope_addr_left(m));
    info_t dr = mrope_cbal_bu(mrope_addr_right(m));
    m->node_u.cat_s.dep = max_long(dl, dr) + 1;
    if (!mrope_is_cbalanced(m)) {
      mrope_t b = mrope_perf_bal(m);
      *p = b;
      return(mrope_depth(b));
    } else {
      return(mrope_depth(m));
    }
  } else {
    abort();
    return 0;
  }
}

/* O(log n) returns a leaf node of mrope m that is picked by taking a
   random walk from the root to the leaf node; or, if m is the empty
   mrope, it returns the empty mrope */
mrope_t
mrope_pick_random_leaf(mrope_t m) {
  node_t t = mrope_node(m);
  if (t == EMPTY) {
    return(mrope_empty());
  } else if (t == LEAF) {
    return m;
  } else if (t == CAT) {
    mrope_t next;
    mrope_t n;
    next = (rand() % 2 == 0) ? 
                mrope_left(m) : mrope_right(m);
    n = mrope_pick_random_leaf(next);
    if (! mrope_is_empty(n)) {
      return n;
    } else {
      next = (next == mrope_left(m) ? 
                mrope_right(m) : mrope_left(m));
      return(mrope_pick_random_leaf(next));
    }
  } else {
    abort();
    return(mrope_empty());
  }
}

/* O(log n) returns the first element of mrope m */
/* if m is the empty mrope, it returns the empty mrope */
mrope_t
mrope_head(mrope_t m) {
  node_t t = mrope_node(m);
  if (t == EMPTY) {
    return(mrope_empty());
  } else if (t == LEAF) {
    return m;
  } else if (t == CAT) {
    mrope_t n = mrope_head(mrope_left(m));
    if (! mrope_is_empty(n))
      return n;
    else
      return(mrope_head(mrope_right(m)));
  } else {
    abort();
    return(mrope_empty());
  }
}

/* O(log n)* inserts data element x just before position i in mrope m
   that is pointed to by p */
/* the * denotes amortized time; worst case time is O(n) */
/* index i is assumed to be in bounds, i.e., 0 <= i < length m */
void
mrope_insert_ib(mrope_t* p, long i, data_t x) {
  mrope_t m = *p;
  node_t t = mrope_node(m);
  if (t == LEAF) {
    *p = mrope_nbcat2(mrope_singleton(x), m);
  } else if (t == CAT) {
    if (i < mrope_length(mrope_left(m))) {
      mrope_insert_ib(mrope_addr_left(m), i, x);
    } else {
      mrope_insert_ib(
	      mrope_addr_right(m), 
	      i - mrope_length(mrope_left(m)), x);
    }
    mrope_finalize(p);
    if (! mrope_is_cbalanced(*p)) {
      /*
      info_t len = mrope_length(*p);
      info_t dep = mrope_depth(*p);
      */
      mrope_t m = mrope_perf_bal(*p);
      *p = m;
      /*      printf("balance olen=%d odep=%d nlen=%d ndep=%d\n", 
	     len, dep, mrope_length(m), mrope_depth(m));
      */
    }
  } else {
    abort();
  }  
}

static long
long_abs(long l) {
  return (l < 0 ? -1l * l : l);
}

data_t 
mrope_rand_data() {
  return(long_abs(data_rand() % 1000));
}

static long
mrope_random_rec(long size, mrope_t* rp_d) {
  if( size >= 2 ) {
    *rp_d = mrope_alloc();
    long n_l = mrope_random_rec( size / 2,   mrope_addr_left(*rp_d) );
    long n_r = mrope_random_rec( size - n_l, mrope_addr_right(*rp_d) );
    mrope_finalize(rp_d);
    return (n_l + n_r);
  }
  else if( size == 1 ) {
    *rp_d = mrope_singleton(mrope_rand_data());
    return 1;
  }
  abort();
  return 0;  
}

mrope_t
mrope_random(long size) {
  mrope_t result;
  mrope_random_rec(size, &result);
  return result;  
}

void
mrope_fprint(FILE* file, mrope_t m) {  
  node_t t = (mrope_node(m));
  if (t == LEAF) {
    fprintf(file, DATA_FMT " ", DATA_FMT_ARGS(mrope_data(m)));
  } else if (t == CAT) {
    mrope_fprint(file, mrope_left(m));
    mrope_fprint(file, mrope_right(m));
  }
}

void
mrope_structure_fprint(FILE* file, mrope_t m) {
  node_t t = mrope_node(m);
  if (t == EMPTY) {
    fprintf(file, " _ ");
  } else if (t == LEAF) {
    fprintf(file, DATA_FMT " ", DATA_FMT_ARGS(mrope_data(m)));
  } else if (t == CAT) {
    fprintf(file, "(");
    mrope_structure_fprint(file, mrope_left(m));
    fprintf(file, ",");
    mrope_structure_fprint(file, mrope_right(m));
    fprintf(file, ")");    
  } else {
    abort();
  }
}

#endif
