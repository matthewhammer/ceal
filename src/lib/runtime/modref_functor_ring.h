/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_ring.h
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

/* Ring modrefs. */

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
/*
  = Ring Modrefs =
  March 29, 2011
  
  == Goal ==

  Lightweight and flexible modrefs.  They can be used where owcr
  modrefs cannot, but (hopefully) have less overhead than modrefs
  based on splay trees.

  == Design ==
  
  Each modref consists of a ring.  The 1-word modref is itself a node
  in the ring.  It points at the next node, if one exists, or itself,
  if the ring contains no other nodes.

  The other nodes, if they exist, each consist of a write or read
  event.  Modref-nodes, read-nodes and write-nodes all have a
  different layout, but they share a common 1-word hdr format that
  allows for insertion, removal and search all in worst-case linear
  time.

  == Analysis ==

  In contexts where a modref has a bounded number of effects (either
  constant, or logarithmic), this modref implementation may be
  effective.  (It has yet to be demonstrated one way or the other.) In
  situations where the number of effects is not sub-linear, this
  implementation should not be used.
  
*/
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Forward declare modref structure. */
struct S(modref);
struct S(hdr);

typedef enum {
  E(TAG_REVOKED_MODREF)  = 0x0, /* 00 */
  E(TAG_MODREF)          = 0x1, /* 01 */
  E(TAG_WRITE)           = 0x2, /* 10 */
  E(TAG_READ)            = 0x3, /* 11 */
  E(TAG_MASK)            = 0x3, /* 11 */
} T(tag);

/* A hdr is 1-word. */
typedef struct S(hdr) {
  union {
    struct S(hdr)* next;
    T(tag)          tag;
    uintptr_t      bits;
  } u;
} T(hdr);

/* An evt is 2-words, (unless CEAL_DEBUG_MODTYP is defined). */
typedef struct S(evt) {
  T(hdr)           hdr;
  ceal_trnode_t*   trnode;
#ifdef CEAL_DEBUG_MODTYP
  ceal_modtyp_t    modtyp;
#endif
} T(evt);

/* 3 Words. */
typedef struct S(readh) {
  T(evt) evt;
  Tv     value;
} T(readh);

/* 3 Words. */
typedef struct S(writeh) {
  T(evt)  evt;
  Tv      value;
} T(writeh);

/* 1 Word. */
typedef struct S(modref) {
  T(hdr) hdr;
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
