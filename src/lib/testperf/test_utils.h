#ifndef __TEST_UTILS__
#define __TEST_UTILS__

#include <stdio.h>
#include <inttypes.h>

char* cheap_sprintf(const char* fmt, ...);
char* strapp2(const char* s1, const char* s2);
char* strapp3(const char* s1, const char* s2, const char* s3);

char* string_of_size(int base, uintptr_t size);

#define LABEL_WIDTH 25
#define mesg_begin(...) fprintf(stderr, "" __VA_ARGS__)
/* #define mesg_begin(...) fprintf(stderr, "\x1b[1;32m" __VA_ARGS__) */
#define mesg(...)     fprintf(stderr, __VA_ARGS__ )
#define mesg_end      fprintf(stderr, "\x1b[0m \n")

/*static int min(int i, int j)
  { return i < j ? i : j; }*/

#endif
