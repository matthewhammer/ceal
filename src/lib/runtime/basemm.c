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

/* Matthew Hammer <hammer@tti-c.org> */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "basemm.h"
#include "freeset.h"
#include "logging.h"

uintptr_t basemm_bytes_malloc = 0;
uintptr_t basemm_bytes_free = 0;
uintptr_t basemm_bytes_maxlive = 0;

#if CEAL_BASEMM_FREESET
static freeset_t global_freeset_ = {0,0,NULL};
freeset_t* global_freeset = &global_freeset_;
#else
#warning CEAL RT will use malloc rather than the (faster) freeset.
#endif

static void*
malloc_and_assert(uintptr_t size) {
  void* ptr = malloc(size);
  if(!ptr) {
    perror("malloc");
    abort();
  }
  return ptr;
}

void basemm_init() {
#if CEAL_BASEMM_FREESET
  {
    static int init_flag = 0;
    if(!init_flag) {
      freeset_init(global_freeset, CEAL_BASEMM_FREESET_MAXBLOCK);
      init_flag = 1;
    }
    else {
      perror("called multiple times");
      abort();
    }
  }
#endif
}

void* basemm_malloc(uintptr_t size) { /*logg("%d", size);*/
#if CEAL_BASEMM_STATS
  basemm_bytes_malloc += size;
  if(basemm_bytes_malloc - basemm_bytes_free > basemm_bytes_maxlive) {
    basemm_bytes_maxlive = basemm_bytes_malloc - basemm_bytes_free;
  }
#endif
#if CEAL_BASEMM_FREESET
  if(size < CEAL_BASEMM_FREESET_MAXBLOCK) {
    return freeset_pop(global_freeset, size);
  }
  else
    return malloc_and_assert(size);
#else
  return malloc_and_assert(size);
#endif
}

void basemm_free(uintptr_t size, void* ptr) {
#if CEAL_BASEMM_STATS
  basemm_bytes_free += size;
#endif
#if CEAL_BASEMM_FREESET
  if(size < CEAL_BASEMM_FREESET_MAXBLOCK) {
    freeset_push(global_freeset, size, ptr);
  }
  else
    free(ptr);
#else
  free(ptr);
#endif
}
