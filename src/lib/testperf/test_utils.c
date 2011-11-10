#include "test_utils.h"
#include <stdlib.h>
#include <stdarg.h>

char* cheap_sprintf(const char* fmt, ...) {
#define BUFFSZ 127
  static char buff[BUFFSZ + 1];
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(buff, BUFFSZ, fmt, ap);
  va_end(ap);  
  return buff;
}

char* strapp2(const char* s1, const char* s2) {
  return cheap_sprintf("%s%s", s1, s2);
}

char* strapp3(const char* s1, const char* s2, const char* s3) {
  return cheap_sprintf("%s%s%s", s1, s2, s3);
}
  
char* string_of_size(int base, uintptr_t size) {  
  uintptr_t _1K;
  uintptr_t _1M;
  uintptr_t _1G;

  if(base == 2) {
    _1K = 1 << 10;
    _1M = 1 << 20;
    _1G = 1 << 30;
  }
  else if(base == 10) {
    _1K = 1000;
    _1M = 1000000;
    _1G = 1000000000;
  }
  else {
    abort();
  }
  
  char* suffix = "";
  float coeff = 0;
  
  if(size < _1K) {
    coeff = size;
    suffix = "";
  }
  else if(size < _1M) {
    coeff = (float)size / (float)_1K;
    suffix = "K";
  }
  else if(size < _1G) {
    coeff = (float)size / (float)_1M;
    suffix = "M";
  }
  else {
    coeff = (float)size / (float)_1G;
    suffix = "G";
  }

  return cheap_sprintf("%.1f%s base-%d", coeff, suffix, base);
}
