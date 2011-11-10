/*
Copyright 2008-2011 
Matthew A. Hammer <hammer@mpi-sws.org>

This file is part of the CEAL language implementation (CEAL for short).

CEAL is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

CEAL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with CEAL.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - */

/* Thursday Oct 7 2010 */

/* Randomness... the emphasis here is *diversity* of random numbers.
   Any repeated random numbers will usually lead to bad stuff
   happening (like poor stability due to overly-aggressive stealing of
   equivalently-keyed allocations).  Hence, we want as many random
   bits as possible, and rand(3) doesn't give very many. */

/* NOT USED. */
uintptr_t random_uintptr_from_dev_urandom() {
  static FILE* f = NULL;

  if(f == NULL) {
    f = fopen("/dev/urandom", "r");

    if(!f) {
      perror("cannot open /dev/urandom");
      exit(-1);
    }
  }

  {
    uintptr_t a;
    fread(&a, sizeof(uintptr_t), 1, f);
    return a;
  }

  /* Minor BUG: we never close the file. */
}

long random_long() {
  return rand();
}

double random_double() {
  return ((double)rand()) / ((double) RAND_MAX);
}


float random_float() {
  return (float) random_double();
}


int random_int() {
  return rand();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Random, sorted indicies (longs). */

/* Some dirty stuff here. */
static long* random_sorted_longs = NULL;


long random_sorted_longs_ith(long i) {
  return random_sorted_longs[i];
}

static int compare_longs(void* x_, void* y_) {
  long* x = (long*) x_;
  long* y = (long*) y_;
  
  if(*x == *y) return 0;
  else if(*x < *y) return -1;
  else if(*x > *y) return 1;
  else abort();
}

void random_sorted_longs_generate(long num, long range) {
  long* arr = random_sorted_longs;
  long i;

  /* Free it, if needbe. */
  if( arr ) {
    free(arr);
  }
  arr = malloc(sizeof(long) * num);

  /* Generate num random numbers */
  for(i = 0; i < num; i++) {
    arr[i] = random_long() % range;
  }

  /* Sort the random numbers */
  qsort(arr, num, sizeof(long), compare_longs);

  /* Save the array for later. */
  random_sorted_longs = arr;
}
