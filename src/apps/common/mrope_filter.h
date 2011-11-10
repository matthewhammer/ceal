
/* Mike Rainey <mrainey@mpi-sws.org> */

/* mrope_filter (m, p, c, d) */
/* O(log n) for each element x of mrope m, it applies the comparison
   operation cmp (c, x, p), and returns a mrope of those x for which
   the predicate evaluated to true, in the same order as they occured
   in the argument mrope */

typedef enum {
  EQUAL,
  LESS,
  GREATER
} cmp_h_t;

typedef long unsigned cmp_t;

bool
cmp(cmp_t c, data_t x, data_t y) {
  if (c == EQUAL)
    return(DATA_EQUAL(x, y));
  else if (c == LESS)
    return(x < y); // FIXME
  else if (c == GREATER)
    return(x > y);
  else {
    abort();
    return 0;
  }
}

void
mrope_nbfilter(mrope_t m, data_t p, cmp_t c, mrope_t* d) {
  node_t t = mrope_node(m);
  if (t == EMPTY) {
    *d = mrope_empty();
  } else if (t == LEAF) {
    if (cmp(c, mrope_data(m), p))
      *d = mrope_singleton(mrope_data(m));
    else
      *d = mrope_empty();
  } else if (t == CAT) {
    *d = mrope_alloc();
    mrope_nbfilter(mrope_left(m), p, c, mrope_addr_left(*d));
    mrope_nbfilter(mrope_right(m), p, c, mrope_addr_right(*d));
    mrope_finalize(d);
  } else {
    abort();
  }
}

void
mrope_filter(mrope_t m, data_t p, cmp_t c, mrope_t* d) {
  mrope_nbfilter(m, p, c, d);
  mrope_cbal_bu(d);
}
