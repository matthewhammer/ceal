/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_owcr.h
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

/* Matthew Hammer <hammer@mpi-sws.org> */

/* One-write, Constant-read Modrefs --- Suitable for modrefs that are
   written exactly once and read only a constant number of times. */

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

/* Forward declare modref structure. */
struct S(modref);

typedef enum {
  E(EVENT_WRITE) = 0x01,
  E(EVENT_READ)  = 0x10,
} T(tag);

/*#define CEAL_MODREF_OWCR_DOUBLE_LINKED*/

#ifdef CEAL_MODREF_OWCR_DOUBLE_LINKED

typedef struct S(node) {
  struct S(node)* next; /* << next field: must be first. */
  struct S(node)* prev;
  T(tag)          tag;  /* << TODO: pack this (in the modref field above). */
#ifdef CEAL_DEBUG_MODTYP
  ceal_modtyp_t   modtyp;
#endif
} T(node);

#else

typedef struct S(node) {
  struct S(node)*   next; /* << next field: must be first. */
  struct S(modref)* modref;
  T(tag)            tag;  /* << TODO: pack this (in the modref field above?) */
#ifdef CEAL_DEBUG_MODTYP
  ceal_modtyp_t     modtyp;
#endif
} T(node);

#endif

typedef struct S(writeh) {
  struct S(node)    node; /* << node field: must be first. */
  Tv                value;
#ifdef CEAL_DEBUG_MODTYP
  ceal_trnode_t*    trnode;
#endif
} T(writeh);

typedef struct S(readh) {
  struct S(node)   node; /* << node field: must be first. */
  Tv               value;
  ceal_trnode_t*   trnode;
} T(readh);

typedef struct S(modref) {
  T(node)* nodes;
} T(modref);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Read interface -- Invoke, Revinv & Revoke */
                        
Tv   F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr);
Tv   F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr);
void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh);
                   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Write interface -- Invoke, Revinv & Revoke */
                        
void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val);
void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val);
void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh);
