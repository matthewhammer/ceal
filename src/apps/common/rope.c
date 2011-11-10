/* Mike Rainey <mrainey@mpi-sws.org>   */
/* Matthew Hammer <hammer@mpi-sws.org> */

#ifndef __ROPE_C__
#define __ROPE_C__

#include <stdio.h>
#include <assert.h>

/* Ropes

  A rope is a balanced binary tree in which data elements are stored
  at leaf nodes. There are three types of rope nodes: empty nodes,
  leaf nodes, and cat nodes. 

    Rope ::= Empty
	   | Leaf Data
	   | Cat Int Int Rope Rope

  The cat nodes cache length and depth information. For a given cat
  node Cat(len, dep, l, r), the length len denotes the number of data
  elements under the cat node, the depth dep denotes the depth of the
  subrope rooted at the cat node, and the ropes l and r denote the
  left and right branches respectively.

 */

typedef struct node_s* rope_t;
typedef rope_t owcr rope_m_t;
typedef long info_t;
typedef long bool;

enum node_e {
  JUNK,
  EMPTY,
  LEAF,
  CAT
};

struct node_s {
  enum node_e n;    /* node identifier */
  union{
    data_t elt;     /* leaf node */
    struct {        /* cat node */
      info_t   len; /* number of leaves under current node */
      info_t   dep; /* depth of subrope rooted at current node */
      rope_m_t l;   /* left branch */
      rope_m_t r;   /* right branch */
    } cat_s;
  } node_u;
};

static long max_long(long a, long b) {
  return (a > b) ? a : b;
}

/* O(1) type of rope rp */
enum node_e
rope_type(rope_m_t rp) {
  if (rp == 0)
    return EMPTY;
  else
    return (rp->n);
}
/* O(1) left branch of rope rp */
rope_m_t
rope_left(rope_m_t rp) {
  return rp->node_u.cat_s.l;
}
/* O(1) right branch of rope rp */
rope_m_t
rope_right(rope_m_t rp) {
  return rp->node_u.cat_s.r;
}

data_t rope_elt(rope_m_t rp) {
  assert(rope_type(rp) == LEAF);
  return rp->node_u.elt;
}

/* O(1) length of rope rp */
long
rope_length(rope_m_t rp) {
  switch(rope_type(rp)) {
  case EMPTY:
    return 0;
  case LEAF:
    return 1;
  case CAT:
    return(rp->node_u.cat_s.len);
  }
  abort();
  return -1;
}
/* O(1) depth of rope rp */
long
rope_depth(rope_m_t rp) {
  switch(rope_type(rp)) {
  case EMPTY:
    return(0);
  case LEAF:
    return(0);
  case CAT:
    return(rp->node_u.cat_s.dep);
  }
  abort();
  return -1;
}

/* O(1) empty rope */
rope_m_t
rope_empty() {
  return 0;
}
/* O(1) singleton rope created from data element elt */
rope_m_t
rope_singleton(data_t elt) {
  rope_m_t r = alloc(struct node_s);
  r->n = LEAF;
  r->node_u.elt = elt;
  return r;
}

/* rope_nccat2 (l, r) */
/* O(1) concatenate two ropes */
/* no attempt is made to coalesce adjacent empty ropes */
rope_m_t
rope_nccat2(rope_m_t l, rope_m_t r) {
  info_t  len = rope_length(l) + rope_length(r);
  info_t  dep = max_long(rope_depth(l), rope_depth(r));
  rope_m_t rp = alloc(struct node_s);
  rp->n = CAT;
  rp->node_u.cat_s.len = len;
  rp->node_u.cat_s.dep = dep;
  rp->node_u.cat_s.l   = l;
  rp->node_u.cat_s.r   = r;
  return rp;
}

/* rope_nbsplit2 (rp, n) */
/* O(log n) splits rope rp into two subropes (l, r) such that l
   contains the first n data elements of rp and r contains the rest */
/* the result ropes (l, r) are not guaranteed to be balanced */
typedef struct {
  rope_m_t l ;
  rope_m_t r ;
} rope_pair_t;

rope_pair_t*
rope_nbsplit2(rope_m_t rp, long n) {
  long len = rope_length(rope_left(rp));
  if (len == n) {
    rope_pair_t split;
    split.l = rope_left(rp);
    split.r = rope_right(rp);
    return &split;
  } else if (n < len) {
    rope_pair_t* split_l = 
		     rope_nbsplit2(rope_left(rp), n);
    rope_pair_t split;
    split.l = split_l->l;
    split.r = rope_nccat2(split_l->r, rope_right(rp));
    return &split;
  } else {
    rope_pair_t* split_r = 
      rope_nbsplit2(rope_right(rp), n - len);
    rope_pair_t split;
    split.l = rope_nccat2(rope_left(rp), split_r->l);
    split.r = split_r->r;
    return &split;
  }
}

static long
rope_random_rec(long size, rope_m_t* rp_d) {
  if( size >= 2 ) {
    rope_m_t left;
    rope_m_t right;       
    long n_l = rope_random_rec( size / 2,   & left );
    long n_r = rope_random_rec( size - n_l, & right );
    rope_m_t rp = rope_nccat2(left, right);    
    *rp_d = rp;
    return (n_l + n_r);
  }
  else if( size == 1 ) {
    *rp_d = rope_singleton(data_rand());
    return 1;
  }
  abort();
  return 0;  
}

rope_m_t
rope_random(long size) {
  rope_m_t result;
  rope_random_rec(size, &result);
  return result;  
}

void
rope_fprint(FILE* file, rope_m_t rp) {  
  switch(rope_type(rp)) {
  case LEAF:
    fprintf(file, DATA_FMT " ", DATA_FMT_ARGS(rope_elt(rp)));
    break;
  case CAT:
    rope_fprint(file, rope_left(rp));
    rope_fprint(file, rope_right(rp));
    break;
  case EMPTY:
    break;
  }
}

rope_m_t
rope_sub_ib_node(rope_m_t rp, long i) {
  switch(rope_type(rp)) {
  case LEAF:
    return rp;
  case CAT:
    if (i < rope_length(rope_left(rp)))
      return rope_sub_ib_node(rope_left(rp), i);
    else
      return rope_sub_ib_node
        ( rope_right(rp), 
          i - rope_length(rope_right(rp)));
  }
}

data_t
rope_sub_ib(rope_m_t rp, long i) {
  switch(rope_type(rp)) {
  case LEAF:
    return(rp->node_u.elt);
  case CAT:
    if (i < rope_length(rope_left(rp)))
      return rope_sub_ib(rope_left(rp), i);
    else
      return rope_sub_ib
        ( rope_right(rp), 
          i - rope_length(rope_right(rp)));
  }
}

/* rope_sub(rp, i) */
/* O(log n) returns the ith data element of rope rp */
data_t
rope_sub(rope_m_t rp, long i) {
  assert(i >= 0 && i < rope_length(rp));
  return rope_sub_ib(rp, i);
}

bool
pred (data_t elt) {
  return 1;
}

#endif
