/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: stack.h
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

/* Matthew Hammer <hammer@tti-c.org> */

/* A Stack that grows in chunks.
 *
 * When a chunk is empty (when all of its items are popped), we save
 * it for later.  The assumption here is that the stack will
 * continuously grow / shrink forever.  No chunks are ever freed back
 * to the basemm.
 */

#ifndef __CEAL_STACK__
#define __CEAL_STACK__

#include <stdint.h>

typedef struct ceal_stack_s ceal_stack_t;
typedef struct ceal_stack_chunk_s ceal_stack_chunk_t;
typedef struct ceal_stack_chunk_hdr_s ceal_stack_chunk_hdr_t;

struct ceal_stack_s {
  ceal_stack_chunk_t* nonempty;
  ceal_stack_chunk_t* allempty;
};

struct ceal_stack_chunk_hdr_s {
  ceal_stack_chunk_t* tail;
  uintptr_t num;  
};

ceal_stack_t* ceal_stack_init(ceal_stack_t* stack);
void* ceal_stack_push(ceal_stack_t* stack, void* item);
void* ceal_stack_pop(ceal_stack_t* stack);
void* ceal_stack_peek(ceal_stack_t* stack);
int   ceal_stack_isempty(ceal_stack_t* stack);

#define CHUNK_SIZE ((4096 - sizeof(ceal_stack_chunk_hdr_t)) / sizeof(void*))

struct ceal_stack_chunk_s {
  ceal_stack_chunk_hdr_t hdr;
  void** items[CHUNK_SIZE];
};

#endif
