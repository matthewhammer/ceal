#ifndef __RANDOM__
#define __RANDOM__

/* - - - - - - - - - - - - - - - - - - - - - - - -  */
/* Generation of random scalars.
   Defined by our runtime library */

extern double random_double();
extern float  random_float();
extern long   random_long();
extern int    random_int();

void random_sorted_longs_generate(long num, long range);
long random_sorted_longs_ith(long i);

#endif
