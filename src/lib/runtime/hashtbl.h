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

/* Matthew Hammer  */
/* <hammer@tti-c.org> */

#ifndef __HASHTBL_H__
#define __HASHTBL_H__

typedef struct bucket_s {
  uintptr_t             hash;
  struct ceal_trnode_s* trnode;
  struct bucket_s*      next;
} bucket_t;

typedef struct hashtbl_s hashtbl_t;

hashtbl_t* hashtbl_init   (hashtbl_t* h, int size);
void       hashtbl_zero   (hashtbl_t* h);
void       hashtbl_insert (hashtbl_t* h, bucket_t* bucket);
void       hashtbl_remove (hashtbl_t* h, bucket_t* bucket);
bucket_t*  hashtbl_lookup (hashtbl_t* h, uintptr_t hash);

struct hashtbl_s {
  int size; /* Always a power of 2 */
  int num;
  bucket_t** table;
};


#include "trace.h"

#endif
