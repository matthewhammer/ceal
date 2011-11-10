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

/* Matthew A Hammer */
/* <hammer@tti-c.org> */

#ifndef __CEAL_FREELIST_H__
#define __CEAL_FREELIST_H__

#include <inttypes.h>

typedef struct freelist_s freelist_t;

void  freelist_init(freelist_t* freelist, uintptr_t block_size);
void* freelist_pop(freelist_t* freelist);
void  freelist_push(freelist_t* freelist, void* ptr);


struct freelist_node_s {
  struct freelist_node_s* next;
};

typedef struct freelist_node_s freelist_node_t;

struct freelist_s {
  uintptr_t block_size;
  freelist_node_t* head;
};

#endif
