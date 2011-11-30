/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_ring.c
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

#include <stdlib.h>
#include <assert.h>

#include "basemm.h"
#include "state.h"
#include "logging.h"

/* RING_STEP_COUNT: For debugging rings that grow too big: */
/* #define CEAL_RING_STEP_COUNT  */
#ifdef CEAL_RING_STEP_COUNT
#define RING_STEP_MAX 8
#define RING_STEP_COUNT(stmt) stmt
#else
#define RING_STEP_COUNT(stmt) ;
#endif

static T(tag) F(hdr_tag)(T(hdr)* hdr) {
  return ((uintptr_t)hdr->u.tag) & ((uintptr_t) E(TAG_MASK));
}

static T(hdr)* F(hdr_next)(T(hdr)* hdr) {
  return (T(hdr)*)
    (((uintptr_t) hdr->u.next) & (~((uintptr_t) E(TAG_MASK))));
}

static void F(hdr_set_bits)(T(hdr)* hdr, T(tag) hdr_tag, T(hdr)* hdr_next) {
  hdr->u.bits = hdr_tag |
    (((uintptr_t) hdr_next) & (~((uintptr_t) E(TAG_MASK))));
}

static void F(hdr_insert_after)( T(hdr)* prev,
                                 T(hdr)* next,
                                 T(tag)  next_tag ) {
  
  F(hdr_set_bits)(next,  next_tag,          F(hdr_next)( prev ) );
  F(hdr_set_bits)(prev,  F(hdr_tag)(prev),  next);
}

static void F(hdr_remove_after)(T(hdr)* prev ) {
#if CEAL_DEBUG
  assert( prev != F(hdr_next)( prev ) );
#endif
  F(hdr_set_bits)(prev, F(hdr_tag)(prev),
                  F(hdr_next)( F(hdr_next)( prev ) ) );
}

static T(evt)* F(evt_of_hdr)(T(hdr)* hdr) {
#if CEAL_DEBUG
  assert( F(hdr_tag)(hdr) == E(TAG_READ) ||
          F(hdr_tag)(hdr) == E(TAG_WRITE) );
#endif
  return (T(evt)*) hdr;
}

static ceal_trnode_t* F(evt_trnode)(T(evt)* evt) {
  return evt->trnode;
}

static void F(evt_set_trnode)(T(evt)* evt, ceal_trnode_t* trnode) {
  evt->trnode = trnode;
}

static T(writeh)* F(writeh_of_hdr)(T(hdr)* hdr) {
#if CEAL_DEBUG
  assert( F(hdr_tag)(hdr) == E(TAG_WRITE) );
#endif
  return (T(writeh)*) hdr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void F(ensure_initialized)(T(modref)* modref) {
  if ( ! modref->hdr.u.bits ) { /* ==> Uninitialized. */
    F(hdr_set_bits)( & modref->hdr, E(TAG_MODREF), & modref->hdr);
  }
  else {
    /* Ok. */
  }
}

void F(assert_initialized)(T(modref)* modref) {
#if CEAL_DEBUG
  assert ( modref->hdr.u.bits );
#endif
}


int F(time_compare)(T(hdr)* here,
                    ceal_trnode_t* there_trnode,
                    T(hdr)*        there_handle) {

  /* Do they have the same trace node? */
  /* If so, compare their offsets. */
  if( F(evt_trnode)( F(evt_of_hdr)(here) ) == there_trnode ) {
    return ((uintptr_t)(here)) - ((uintptr_t)(there_handle));
  }
  /* Different trace nodes:
     Compare their trace nodes' times */
  else {
    return totalorder_compare
      (ceal_trnode_time( F(evt_trnode)(F(evt_of_hdr)( here )) ),
       ceal_trnode_time( there_trnode ));
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Read interface -- Invoke, Revinv & Revoke */
                        
Tv F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {
  T(modref)* modref = (T(modref)*) ptr;
  T(writeh)* writeh = NULL;
    
  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " R- handle %p, pointer %p",
       VAS_TRNODE(trnode),    readh,     ptr);

  /* Is the modref holding NULL? */
  F(assert_initialized)(modref);

  T(hdr)* h_modref  = & modref->hdr;
  T(hdr)* h_prev    = h_modref;
  T(hdr)* h_here    = F(hdr_next)( h_modref );

  /* If we are doing a meta-level read, then the time we want is
     either the very last time (ceal_state->last) or, in the case that
     the modref is dirty, the first time (ceal_state->first). */
  /* TODO -- if the modref is dirty, use ceal_state->first below. */
  ceal_trnode_t* trnode_ = trnode ? trnode : ceal_state->last;

  RING_STEP_COUNT(long steps = 0);
  
  /* Search for the write that we should see. */
  while( h_here != h_modref
         &&
         F(time_compare)( h_here, trnode_, & readh->evt.hdr ) < 0 )
  {
    if( F(hdr_tag)( h_here ) == E(TAG_WRITE) )
      writeh = F(writeh_of_hdr)( h_here );

    h_prev = h_here;
    h_here = F(hdr_next)( h_here );

    RING_STEP_COUNT(steps++; assert(steps <= RING_STEP_MAX));
  }

  if( writeh == NULL ) {
    assert(!("expecting to find a write." ));
    abort();
  }
  else {
    if( readh ) {
      /* Insert readh immediately after "prev" */
      F(hdr_insert_after)(h_prev, & readh->evt.hdr, E(TAG_READ));
      F(evt_set_trnode)(& readh->evt, trnode);
      readh->value = writeh->value;
    }
  }
  
  /* Logging Post Conditions */
  logg(FMT_TRNODE        " R+ handle %p, pointer %p, value " Fv,
       VAS_TRNODE(trnode),    readh,     ptr,        writeh->value);

  return writeh->value;
}


Tv F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {
  /* We cannot detect if its the same modref or not (we dont store this info).
     So, nothing special to do here. */
  F(read_revoke)(trnode, readh);
  return F(read_invoke)(trnode, readh, ptr);
}


void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh) {
  /* Search for readh.  Remove it. */

  T(hdr)* h_read = & readh->evt.hdr;
  T(hdr)* h_prev = h_read;
  T(hdr)* h_here = F(hdr_next)( h_prev );

  RING_STEP_COUNT(long steps = 0);
  
  /* Look for the predecessor hdr, and the predecessor write, if any */
  while( h_here != h_read ) {

    if( F(hdr_tag)( h_here ) == E(TAG_REVOKED_MODREF) ) {
      /* Modref has been revoked.  No need to go further. */
      return;
    }

    h_prev = h_here;
    h_here = F(hdr_next)( h_here );
    
    RING_STEP_COUNT(steps++; assert(steps <= RING_STEP_MAX));
  }

  /* Remove it. */
  F(hdr_remove_after)( h_prev );
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Write interface -- Invoke, Revinv & Revoke */

void F(enqueue_affected_readers) (ceal_trnode_t* trnode,
                                  T(writeh)* writeh,
                                  T(hdr)* hdr_first) {
  T(hdr)* hdr = hdr_first;
  
  while( F(hdr_tag)( hdr ) == E(TAG_READ) )
  {
    T(readh)* readh = (T(readh)*) F(evt_of_hdr)( hdr );
    
    if ( writeh->value == readh->value ) {
      logg("no change to read: %p", readh);
      /* All the other reads also have same value.
         So, we are done. */
      break; 
    }
    else {
      logg("enqueuing read: %p", readh);
      ceal_trnode_t* readh_trnode = F(evt_trnode)( & readh->evt );
      /* Does the successor occur (strictly) after this write? */
      if( trnode != readh_trnode ) {
        /* If so, there's no need to enqueue these reads--
           we will necessarily evaluate them
           (by virtue of them being in the same trace node). */
        ceal_trnode_enqueue( readh_trnode );
      }
    }
    
    hdr = F(hdr_next)( hdr );
  }
}

void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {

  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " W- handle %p, pointer %p, value " Fv,
       VAS_TRNODE(trnode), writeh,       ptr,        val);

  T(modref)* modref = (T(modref)*) ptr;

  /* Is the modref holding NULL? */
  F(ensure_initialized)(modref);
  
  T(hdr)* h_modref  = & modref->hdr;
  T(hdr)* h_prev    = h_modref;
  T(hdr)* h_here    = F(hdr_next)( h_prev );
  
  /* Is the write meta-level? */
  if( !trnode ) {
    /* Case: Meta-Level write. */    
    ceal_trnode_t* first = ceal_state->first;
    
#if CEAL_DEBUG
    assert( ! writeh );
#endif
    /* Invariant: if a meta-level write exists, then it is _first_ in
       the ring, immediately after the modref. */
    if( F(hdr_tag)( h_here ) == E(TAG_WRITE) &&
        ( F(evt_trnode)( F(evt_of_hdr)( h_here ) ) == first) ) {
      writeh = F(writeh_of_hdr)( h_here );
    }
    
    /* None found. Create a new one. Insert it. */
    if(! writeh ) {
#if 0
      writeh = basemm_malloc(sizeof(T(writeh)));
#else
      /* Use the metaboxes interface for allocation; This is a
         stop-gap measure until we have a better representation of
         modrefs, such that meta-level writes are unboxed (within the
         modref representation itself). */
      writeh = ceal_alloc_invoke(NULL, NULL, sizeof(T(writeh)));
#endif
      F(hdr_insert_after)( h_modref, & writeh->evt.hdr, E(TAG_WRITE));
      F(evt_set_trnode)(& writeh->evt, first );
    }

    /* Set the written value. */
    writeh->value = val;
  }
  else {
    /* Case: Core-Level write. */
    
    RING_STEP_COUNT(long steps = 0);
    
    /* Search for the position where we should insert. */
    while( h_here != h_modref &&
           F(time_compare)( h_here, trnode, & writeh->evt.hdr ) < 0 )
    {
      h_prev = h_here;
      h_here = F(hdr_next)( h_here );

      RING_STEP_COUNT(steps++; assert(steps <= RING_STEP_MAX));
    }
    
    /* Insert it. */
    F(hdr_insert_after)(h_prev, & writeh->evt.hdr, E(TAG_WRITE));
    F(evt_set_trnode)(& writeh->evt, trnode);
    writeh->value = val;
  }
  
  /* Enqueue any affected readers that come afterward. */
  F(enqueue_affected_readers)( trnode, writeh, F(hdr_next)( & writeh->evt.hdr ) );

  logg(FMT_TRNODE        " W+ handle %p, pointer %p, value " Fv "",
       VAS_TRNODE(trnode),    writeh,    ptr,        val);

}

void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {
  /* We cannot detect if its the same modref or not (we dont store this info).
     So, nothing special to do here. */
  F(write_revoke)(trnode, writeh);
  F(write_invoke)(trnode, writeh, ptr, val);
}

void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh) {

  T(hdr)* h_write = & writeh->evt.hdr;
  T(hdr)* h_prev  = h_write;
  T(hdr)* h_here  = F(hdr_next)( h_prev );

  /* Keep track of what we see when we traverse the ring. */
  int         seen_modref = 0;
  T(writeh)*  prev_write = NULL;    

  RING_STEP_COUNT(long steps = 0);

  /* Look for the predecessor hdr, and the predecessor write, if any */
  while( h_here != h_write ) {

    if( F(hdr_tag)( h_here ) == E(TAG_REVOKED_MODREF) ) {
      /* Modref has been revoked.  No need to go further. */
      return;
    }
    
    if( seen_modref && F(hdr_tag)( h_here ) == E(TAG_WRITE) ) {
      /* Found a preceeding write.  Remember it. */
      prev_write = F(writeh_of_hdr)( h_here );
    }
    else if( F(hdr_tag)( h_here ) == E(TAG_MODREF) ) {
      /* Recall that we have seen the modref;
         hence, we've wrapped around in the ring. */
      seen_modref = 1;
    }

    h_prev = h_here;
    h_here = F(hdr_next)( h_here );

    RING_STEP_COUNT(steps++; assert(steps <= RING_STEP_MAX));
  }

#if CEAL_DEBUG
  assert( seen_modref &&
          "every trip to our predecessor goes through the modref");
#endif
    
  /* Remove it. */
  F(hdr_remove_after)( h_prev );

  /*if( !h_prevw || (h_prevw && h_prevw->value != writeh->value )) {*/
  if( prev_write && prev_write->value != writeh->value ) {
    /* DESIGN DECISION: We do not enqueue readers if there is no
       previous write. */    
    /* Enqueue any readers that come afterward. */
    F(enqueue_affected_readers)( trnode, prev_write, F(hdr_next)( & writeh->evt.hdr ) );
  }  
}
