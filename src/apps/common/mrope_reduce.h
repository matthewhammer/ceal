/* Mike Rainey <mrainey@mpi-sws.org> */

/* mrope_reduce (m, a, z) */
/* O(log n) */

typedef enum {
  SUM,
  MAX
} assoc_op_h_t;

typedef long unsigned assoc_op_t;

data_t
assoc_op(assoc_op_t a, data_t x, data_t y) {
  if (a == SUM)
    return data_add(x, y);
  else if (a == MAX)
    return data_max(x, y);
  else {
    abort();
    return x;
  }
}

void
mrope_reduce(mrope_t m, assoc_op_t a, data_t z, data_t* d) {
  node_t t = mrope_node(m);
  if (t == EMPTY) {
    *d = z;
  } else if (t == LEAF) {
    *d = mrope_data(m);
  } else if (t == CAT) {
    data_t l;
    data_t r;
    mrope_reduce(mrope_left(m), a, z, &l);
    mrope_reduce(mrope_right(m), a, z, &r);
    *d = m->node_u.cat_s.c = assoc_op(a, l, r);
  } else {
    abort();
  }
}
