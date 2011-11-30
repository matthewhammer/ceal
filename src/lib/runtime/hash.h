/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: hash.h
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

#ifndef __CEAL_HASH_H__
#define __CEAL_HASH_H__

#include <inttypes.h>
#include <stdint.h>

/* The following routines are customized statically for the word-size
   of the machine. */

uintptr_t
hash_oneword(uintptr_t word);

uintptr_t
hash_buffer(unsigned char* bytes,
            uintptr_t bytec,
            uintptr_t hashin);
#endif
