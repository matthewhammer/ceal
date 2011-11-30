/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_awar.h
Author(s):
  Matthew A. Hammer <hammer@mpi-sws.org>
  Yan Chen <chenyan@mpi-sws.org>

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

/* Splay-Tree-based Modrefs --- Suitable for modrefs that are written
   and read an arbitrary number of times. */

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
typedef struct S(event) {

#ifdef CEAL_DEBUG_MODTYP
  ceal_modtyp_t modtyp;
#endif

  struct S(event)* left;
  struct S(event)* right;
  
  ceal_trnode_t* trnode;

  enum {
    E(EVENT_READ),
    E(EVENT_WRITE),
  } tag; /* TODO: pack this, perhaps in the modref field? */

  struct S(modref)* modref;
  
  Tv value;
  
} T(event);

typedef struct S(modref) {
  union {
    T(event)* root;
    uintptr_t bits; /* Use LSBs of root pointer for extra info. */
    /* Bit 0: Unused.
       Bit 1:
       Set   --> modref is inconsistent with core;
             ==> meta-level reads see meta-level write.
       Unset --> modref is consistent with core;
             ==> meta-level reads see last write in (meta;core).
    */
  } u;
} T(modref);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Read interface -- Invoke, Revinv & Revoke */
typedef struct S(event) T(readh);
Tv   F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr);
Tv   F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr);
void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh);
                   
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Write interface -- Invoke, Revinv & Revoke */
typedef struct S(event) T(writeh);
void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val);
void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val);
void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh);
