/* Matthew Hammer <hammer@ttic.edu> */

/* array_reduce: Given a non-empty array of elements in some monoid,
   reduces the array to a single element (via sampling) and returns
   this resulting element. */

/* Note: Use C preprocessor to specify args:

   Monoid_t    -- list element type
   Monoid_fun  -- monoid operation over list elements
   Reduce_fun  -- name of function being defined below
   
   This is essentially a cumbersome syntax for C++ templates, except
   that we don't have to use C++.
*/

Monoid_t Reduce_fun(Monoid_t* arr, long len, void* op_env) {
  if(len > 0) {

    while(len > 1) {

      for(long i = 0; i < len; i += 2) {
        cut {
          if( i + 1 < len ) {
            arr[i / 2] =
              Monoid_fun(op_env, arr[i], arr[i + 1]);
          }
          else {
            arr[i / 2] =
              arr[i];
          }
        }
      }

      len = (len / 2) + (len % 2);
              
    }
    return arr[0];
  }
  else {
    abort();
    return 0;
  }  
}
