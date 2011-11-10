/* The default behavior of CIL, i.e., Cil.insertImplicitCasts = true,
   allows the following code to type-check (with the exception of the
   line commented out with "CIL ERROR") */

/* When Cil.insertImplicitCasts is false, the code still passes CIL,
   but in IL we witness and report the type errors on the lines
   indicated by the comments "IL-ERROR". */

void incr_int_ptr(int* x) {
  (*x)++;
}

int main(int argc, char** argv) {

  /* too many stars on x */
  int** x = alloc(int);  /* off - IL-ERROR */

  incr_int_ptr(x);       /* off - IL-ERROR */
  incr_int_ptr(*x);      /* Ok. */
  incr_int_ptr(**x);     /* off - IL-ERROR */
                           
  /* too few stars on y */
  int*  y = alloc(int*); /* off - IL-ERROR */

  incr_int_ptr(y);       /* Ok. */
  incr_int_ptr(*y);      /* off - IL-ERROR */
  
  /*incr_int_ptr(**y); *//* CIL ERROR */

  /* too few stars on z */
  int** z = alloc(int**); /* off - IL-ERROR */

  incr_int_ptr(z);        /* off - IL-ERROR */
  incr_int_ptr(*z);       /* Ok. */
  incr_int_ptr(**z);      /* off - IL-ERROR */

}
