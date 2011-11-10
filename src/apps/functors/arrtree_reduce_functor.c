/* Matthew Hammer <hammer@ttic.edu> */

/* arrtree_reduce: Given a non-empty array-tree of elements in some
   monoid, reduces the array-tree to a single element (via sampling)
   and returns this resulting element. */

/* Note: Use C preprocessor to specify args:

   Monoid_t    -- list element type
   Monoid_fun  -- monoid operation over its elements
   Array_fun   -- function to reduce arrays to monoid elements
   Reduce_fun  -- name of function being defined below
   
   This is essentially a cumbersome syntax for C++ templates, except
   that we don't have to use C++.
*/

Monoid_t Reduce_fun(arrtree_t t, void* op_env, Monoid_t id_element) {
  if(t == NULL)
    return id_element;
  else {
    return
      Monoid_fun(op_env,
                 Array_fun(t->arr, t->len, op_env),
                 Monoid_fun(op_env,
                            Reduce_fun(t->left, op_env, id_element),
                            Reduce_fun(t->right, op_env, id_element)));
  }
}
