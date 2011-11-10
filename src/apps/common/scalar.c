#ifndef __SCALAR_C__
#define __SCALAR_C__

#ifndef SCALAR_DATA
#error  SCALAR_DATA is not defined
#endif

#ifndef SCALAR_DATA_TYPES
#error  SCALAR_DATA_TYPES is not defined
#endif

#include <stdlib.h>
#include <math.h>
#include "random.h"

/* - - - - - - - - - - - - - - - - - - - - - - - -  */

#if SCALAR_DATA == SCALAR_DOUBLE
typedef double data_t;
#define DATA_FMT "%.2e"
#define DATA_FMT_ARGS(a) (a)
#define DATA_EQUAL(a, b) (fabs((a) - (b)) < 0.0000001)
#define DATA_COMP(a, b) ((a) <= (b))
double data_rand() { return random_double(); }
double data_ith(long i) { return (double) i; }
double data_add(double a, double b) { return a + b; }
double data_max(double a, double b) { return a > b ? a : b; }
double data_min(double a, double b) { return a < b ? a : b; }

/* - - - - - - - - - - - - - - - - - - - - - - - -  */

#elif SCALAR_DATA == SCALAR_BOXED_DOUBLE
/* Boxed doubles */

/* Boxing scalar data gives it a distinct/unique identity.  This
   property becomes essential when we use the (boxed) data to key
   allocations in the self-adjusting program.  Because of the way we
   now support allocation stealing (as just another use of ordinary
   memoization) it is essential that we reuse allocations
   monotonically.  Giving them unique identities makes this easy.

   As an experiment, try running a simple program (list_copy) with
   lists of keys being either doubles or _boxed_ doubles.  You'll find
   that when the number of input keys gets sufficiently high, the
   psuedo-random doubles are no longer sufficiently distinct to yield
   scalable speedups.  OTOH, since boxes each have a distinct identity
   (their address), this problem completely vanishses and the speedups
   scale.
*/


typedef double persistent * data_t;
#error I think that this is outdated

#define DATA_FMT "%.2e"
#define DATA_FMT_ARGS(a) (*a)
#define DATA_EQUAL(a, b) (fabs((*a) - (*b)) < 0.0000001)
#define DATA_COMP(a, b) ((*a) <= (*b))

data_t data_rand() {
  data_t d = alloc(double);
  *d = random_double();
  return d;
}

data_t data_add(data_t a, data_t b) {
  data_t d = alloc(double);
  *d = *a + *b;
  return d;
}

data_t data_max(data_t a, data_t b) {
  return *a > *b ? a : b;
}

data_t data_min(data_t a, data_t b) {
  return *a < *b ? a : b;
}

/* - - - - - - - - - - - - - - - - - - - - - - - -  */

#elif SCALAR_DATA == SCALAR_FLOAT
typedef float data_t;
#define DATA_FMT "%f"
#define DATA_FMT_ARGS(a) (a)
#define DATA_EQUAL(a, b) (fabsf((a) - (b)) < 0.0000001)
#define DATA_COMP(a, b) ((a) <= (b))
float data_rand() { return random_float(); }
float data_ith(long i) { return (float) i; }
float data_add(float a, float b) { return a + b; }
float data_max(float a, float b) { return a > b ? a : b; }
float data_min(float a, float b) { return a < b ? a : b; }

/* - - - - - - - - - - - - - - - - - - - - - - - -  */

#elif SCALAR_DATA == SCALAR_LONG
typedef long data_t;
#define DATA_FMT  "%02ld"
#define DATA_FMT_ARGS(a) (a)
#define DATA_EQUAL(a, b) (a == b)
#define DATA_COMP(a, b) ((a) <= (b))
long data_rand() { return random_long(); }
long data_ith(long i) {
  int r = random_int();
  long j = (i << 32) + r;
  return j;
}
long data_add(long a, long b) { return a + b; }
long data_max(long a, long b) { return a > b ? a : b; }
long data_min(long a, long b) { return a < b ? a : b; }

/* - - - - - - - - - - - - - - - - - - - - - - - -  */

#elif SCALAR_DATA == SCALAR_INT
typedef int data_t;
#define DATA_FMT "%2d"
#define DATA_FMT_ARGS(a) (a)
#define DATA_EQUAL(a, b) (a == b)
#define DATA_COMP(a, b) ((a) <= (b))
int data_rand() { return random_int(); }
int data_ith(long i) { return (int) i; }
int data_add(int a, int b) { return a + b; }
int data_max(int a, int b) { return a > b ? a : b; }
int data_min(int a, int b) { return a < b ? a : b; }

/* - - - - - - - - - - - - - - - - - - - - - - - -  */

#elif SCALAR_DATA == SCALAR_STRING
#include <string.h>
typedef char* data_t;
#define DATA_FMT "%s"
#define DATA_FMT_ARGS(a) (a)
#define DATA_EQUAL(a, b) (strcmp((a), (b)) == 0)
#define DATA_COMP(a, b) (strcmp((a), (b)) <= 0)

const char* data_ith(long i) {
  const size_t STRING_SIZE = 33;
  char* buff = malloc(STRING_SIZE);
#if 1
#warning CEAL_DEBUG: Making shorter strings, for readibility during debugging.
  snprintf(buff, STRING_SIZE, "%d", (int)i);
#else
  snprintf(buff, STRING_SIZE, "%032d", (int)i);
#endif
  return buff;
}

const char* data_rand() {
  const size_t STRING_SIZE = 33;
  char* buff = malloc(STRING_SIZE);
  snprintf(buff, STRING_SIZE, "%032d", rand());
  return buff;
}

#else
#error This should not happen.
#endif

#endif
