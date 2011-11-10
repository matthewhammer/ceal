/* Matthew Hammer <hammer@mpi-sws.org> */

/* list_reduce: Given a non-empty list of elements in some monoid,
   reduces the list to a single element (via sampling) and returns
   this resulting element. */

#if  defined(List_t)       \
  && defined(List_tl_t)    \
  && defined(List_hd_t)    \
  && defined(List_cons)    \
  && defined(List_hd)      \
  && defined(List_tl)      \
  && defined(Monoid_binop) \
  && defined(Reduce_fun)
/* Then Ok. */
#else
#error Undefined functor arguments.
#endif

#include "coin.c"

List_hd_t Reduce_fun(List_tl_t* in_, void* op_env) {
  List_t* in;

  cut {
    /* Copy the input to a fresh allocation.  We side-effect this
       allocation as we run.  (Side-effecting the input itself is not
       desirable--it requires that it have AWAR capability).
    */
    in = alloc(List_t);
    *in = *in_;
  }
  
  /* While the input contains more than one element... */
  while(1) {
    long at_basecase = 0;
    
    cut {
      List_t l = *in;
      
      if(l == NULL)
        abort();
      
      else {        
        /* Check for the base case:
           Only 1 element in list. */
        at_basecase = ( (*List_tl(l)) == NULL );
      }
    }    

    if(at_basecase) {
      List_t     c = *in;
      List_hd_t hd = List_hd(c);
      return hd;
    }

    else {
      
      /* The list has several elements.
         Reduce its length by (stable) sampling. */
      memo(fresh_scope){
        List_t*    next_in = in;
        List_t     in      = *next_in;
        coin_t*    coin    = coin_biased_4();
        List_tl_t* out     = alloc(List_tl_t);
        List_tl_t* tail    = out;
        
        /* Loop until list is empty... */
        while(1) {
          List_hd_t hd = List_hd(in);
          
          /* Sample some more elements for hd. */
          while(in = *List_tl(in),
                in && coin_flip(coin, (void*) in)) {
            hd = Monoid_binop(op_env, hd, List_hd(in));
          }
          
          /* Append hd to the next list */
          List_t c = memo(List_cons(hd));
          
          memo;
          
          /* Advance the tail pointer. */
          *tail = c;
          tail = List_tl(c);
          
          /* Are we at the end? */
          if(in == NULL) {
            *tail = NULL;
            break;
          }
          else
            continue;
        }

        /* Process the output list as input. */
        *next_in = *out;
      }
      continue;
    }
  }
}
