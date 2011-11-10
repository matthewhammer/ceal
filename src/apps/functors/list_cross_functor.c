/* Matthew Hammer <hammer@ttic.edu> */

/* list_reduce: Given two input lists representing sets A and B,
   creates a list representing their cross product A x B.  There is no
   special handling for non-unique elements.*/

#if  defined(List_in_t)      \
  && defined(List_in_tl_t)   \
  && defined(List_in_hd)     \
  && defined(List_in_tl)     \
  && defined(List_out_t)     \
  && defined(List_out_tl_t)  \
  && defined(List_out_cons)  \
  && defined(List_out_tl)    \
  && defined(List_cross)
/* Then Ok. */
#else
#error Undefined functor arguments.
#endif

void List_cross(List_in_t in1, List_in_tl_t* in2, List_out_tl_t* d) {

  if(in1 == NULL) {
    *d = NULL;
  }
  else {
    List_in_t in2_ = *in2;

    while(in2_ != NULL) {
      List_out_t c = List_out_cons( List_in_hd(in1), List_in_hd(in2_) );
      memo;
      *d = c, d = List_out_tl(c);
      in2_ = *List_in_tl(in2_);
    }

    memo;
    List_cross(*List_in_tl(in1), in2, d);
  }
}
