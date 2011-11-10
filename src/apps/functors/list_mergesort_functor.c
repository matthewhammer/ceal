/* Matthew Hammer <hammer@mpi-sws.org> */

#if  defined(List_t)       \
  && defined(List_tl_t)    \
  && defined(List_hd_t)    \
  && defined(List_cons)    \
  && defined(List_hd)      \
  && defined(List_tl)      \
  && defined(Hd_less_than) \
  && defined(Ng)           \
  /* Then Ok. */
#else
#error Undefined functor arguments.
#endif

#include "coin.c"

static void
Ng(split)(coin_t* coin, List_t in, List_tl_t* out1, List_tl_t* out2) {
  
  if(! in ) {
    *out1 = NULL;
    *out2 = NULL;
  }
  else {
    List_hd_t h = List_hd(in);
    List_t    c = memo(List_cons(h));
    
    if(coin_flip(coin, (void*) in)) {
      memo;
      *out1 = c;
      Ng(split)(coin, *List_tl(in), List_tl(c), out2);
    }
    else {
      memo;
      *out2 = c;
      Ng(split)(coin, *List_tl(in), out1, List_tl(c));
    }
  }  
}

static void
Ng(merge)(List_t in1, List_t in2, List_tl_t* out) {
  if(!in1)
    *out = in2;

  else if(!in2)
    *out = in1;

  else {
    List_hd_t h1 = List_hd(in1);
    List_hd_t h2 = List_hd(in2);
    
    if( Hd_less_than (h1, h2) ) { 
      List_t c = memo(List_cons(h1));
      memo;
      *out = c;
      Ng(merge)(*List_tl(in1), in2, List_tl(c));
    }
    else { 
      List_t c = memo(List_cons(h2));
      memo;
      *out = c;
      Ng(merge)(in1, *List_tl(in2), List_tl(c));
    }    
  }  
}

static void
Ng(mergesort)(List_tl_t* in, List_tl_t* out) {  

  long at_basecase = 0;

  cut {
    List_t list_in = *in;
    at_basecase = 
      ( list_in == NULL
        ? 1
        : *List_tl(list_in) == NULL );
  }
  
  if( at_basecase ) {
    *out = *in;
  }
  else {    
    List_tl_t l1, l1_sorted;
    List_tl_t l2, l2_sorted;
    
    memo(fresh_scope){
      Ng(split)(coin_fair(), *in, &l1, &l2);
    }

    Ng(mergesort)(&l1, &l1_sorted);
    Ng(mergesort)(&l2, &l2_sorted);

    memo(fresh_scope) {
      Ng(merge)(l1_sorted, l2_sorted, out);
    }
  }
}
