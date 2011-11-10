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

/* Matthew A Hammer <hammer@tti-c.org> */
/* Base Memory Management -- explicit, non-moving memory management,
   optimized for lots of small objects. */

#ifndef __CEAL_BASEMM_H__
#define __CEAL_BASEMM_H__

#include <inttypes.h>

extern uintptr_t basemm_bytes_malloc;
extern uintptr_t basemm_bytes_free;
extern uintptr_t basemm_bytes_maxlive;

void  basemm_init();
void* basemm_malloc(uintptr_t size);
void  basemm_free(uintptr_t size, void* ptr);

#endif
