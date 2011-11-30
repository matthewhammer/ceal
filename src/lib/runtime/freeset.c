/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: freeset.c
Author(s): Matthew A. Hammer <hammer@mpi-sws.org>

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

/* Matthew A Hammer <hammer@tti-c.org */

#include "freeset.h"
#include <stdlib.h>
#include <assert.h>

/* A freeset is implemented as an array of freelists where the ith
 * slot of the array corresponds to a block-size of i words for the
 * ith freelist, stored there.
 *
 * The role of the freeset is to initialize and then multiplex into
 * this array of freelists.
 */

static uintptr_t
nwords_for_nbytes(uintptr_t nbytes) {
  return ((nbytes / sizeof(void*)) + (nbytes % sizeof(void*) ? 1 : 0));
}

static freelist_t*
freelist_for_nwords(freeset_t* freeset, uintptr_t nwords) {
  int i = nwords - 1;
  assert(i < freeset->freelistc);
  return (&((freeset)->freelistv[i]));
}

void  freeset_init(freeset_t* freeset, uintptr_t max_block_size) {
  int freelistc = nwords_for_nbytes(max_block_size);
  assert(freelistc < 256);  
  freeset->freelistc = freelistc;
  freeset->freelistv = malloc(sizeof(freelist_t) * freelistc);

  {int i; for(i = 1; i < freelistc; i++) {
    freelist_t* freelist = freelist_for_nwords(freeset, i);
    freelist_init(freelist, i * sizeof(void*));
  }}
}

void* freeset_pop(freeset_t* freeset, uintptr_t size) {
  if(!size) {
    return NULL;
  }
  else {
    int words = nwords_for_nbytes(size);
    freelist_t* freelist = freelist_for_nwords(freeset, words);
    return freelist_pop(freelist);
  }
}

void  freeset_push(freeset_t* freeset, uintptr_t size, void* ptr) {
  if(!size || !ptr) {
    return;
  }
  else {
    int words = nwords_for_nbytes(size);
    freelist_t* freelist = freelist_for_nwords(freeset, words);
    freelist_push(freelist, ptr);
  }
}
