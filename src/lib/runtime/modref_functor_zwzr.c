/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_zwzr.c
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

/*
 * Matthew Hammer <hammer@ttic.edu>
 */

#include <stdlib.h>
#include <assert.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Read interface -- Invoke, Revinv & Revoke */

Tv F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {
  T(modref)* modref = (T(modref)*) ptr;
  return modref->value;
}

Tv F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {
  return F(read_invoke)(trnode, readh, ptr);
}

void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh) {
  /* Nop. */
}
                   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Write interface -- Invoke, Revinv & Revoke */

void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {
  T(modref)* modref = (T(modref)*) ptr;
  modref->value = val;
}

void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {
  F(write_invoke)(trnode, writeh, ptr, val);
}

void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh) {
  /* Nop. */  
}
