/* Mike Rainey <mrainey@mpi-sws.org> */

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_LONG
#include "scalar.c"

#include "mrope.c"
#include "mrope_input.c"
#include "mrope_output.c"
#include "main.c"

static mrope_pair_t*
split2(data_t x, mrope_t m) {
  enum node_e t = mrope_node(m);
  mrope_pair_t s;
  if (t == EMPTY) {
    s.l = mrope_empty();
    s.r = mrope_empty();
  } else if (t == LEAF) {
    if (x < mrope_data(m)) {
      s.l = mrope_empty();
      s.r = m;
    } else {
      s.l = m;
      s.r = mrope_empty();
    }
  } else if (t == MCAT) {
    data_t k = mrope_data(m);
    if (x == k) {
      s.l = mrope_left(m);
      s.r = mrope_right(m);
    } else if (x < k) {
      mrope_pair_t* r = split2(x, mrope_left(m));
      s.l = r->l;
      s.r = mrope_mnbnccat2(k, r->r, mrope_right(m));
    } else {
      mrope_pair_t* r = split2(x, mrope_right(m));
      s.l = mrope_mnbccat2(k, mrope_left(m), r->l);
      s.r = r->r;
    }
  } else {
    abort();
  }
  return &s;
}

mrope_t
mrope_merge(mrope_t x, mrope_t y) {
  enum node_e t = mrope_node(x);
  if (t == EMPTY) {
    return y;
  } else if (t == LEAF) {
    data_t c = mrope_data(x);
    if (mrope_data(y) > c)
      return(mrope_mnbnccat2(c, mrope_empty(), y));
    else
      return(mrope_mnbnccat2(c, y, mrope_empty()));
  } else {
    data_t c = mrope_data(x);
    mrope_pair_t* y_s = split2(c, y);
    return(mrope_mnbnccat2(
	     c,
	     mrope_merge(mrope_left(x),  y_s->l), 
	     mrope_merge(mrope_right(x), y_s->r)));
  }
}
