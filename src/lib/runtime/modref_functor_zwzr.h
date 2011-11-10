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

/*
 * Matthew Hammer <hammer@mpi-sws.org>
 */

/* Modrefs that record Zero-Writes and Zero-Reads (ZWZR). */

#if defined(Tv) \
 && defined(S)  \
 && defined(T)  \
 && defined(F)  \
 && defined(E)
/* then Ok */
#else
#error Undefined functor arguments.
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Modref structure */

typedef struct S(modref) {
  Tv value;
} T(modref);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Read interface -- Invoke, Revinv & Revoke */

typedef struct S(readh) {
  /* Nothing. */  
} T(readh);

Tv   F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr);
Tv   F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr);
void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh);
                   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Write interface -- Invoke, Revinv & Revoke */
                   
typedef struct S(writeh) {
  /* Nothing. */
} T(writeh);

void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val);
void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val);
void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh);
