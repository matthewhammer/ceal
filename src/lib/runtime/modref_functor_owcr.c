/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_owcr.c
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

/* Get the tag -- this may require unpacking. */
static T(tag) F(node_tag)(T(node)* node);

/* Get the next -- this may require unpacking. */
static T(node)* F(node_next)(T(node)* node);

static void
F(node_insert)(T(modref)* modref,
               T(node)** next_ptr,
               T(node)* node,
               T(tag) tag);

static void
F(node_remove)(T(node)* node);

/* A conservative check: it's always safe to return false (0). */
static int
F(node_has_same_modref)(T(node)* node, T(modref)* modref);
              
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

/* Do a cast with some debugging checks. */
static T(readh)* F(readh_of_node)(T(node)* node) {  
  T(readh)* readh = (T(readh)*) node;
#if CEAL_DEBUG_MODTYP  
  assert( node->modtyp == CEAL_MODTYP_RING );
  assert( F(node_tag)(node) == E(EVENT_READ) );
#endif  
  return readh;
}                       

/* Do a cast with some debugging checks. */
static T(writeh)* F(writeh_of_node)(T(node)* node) {
  T(writeh)* writeh = (T(writeh)*) node;
#if CEAL_DEBUG_MODTYP  
  assert( node->modtyp == CEAL_MODTYP_RING );
  assert( F(node_tag)(node) == E(EVENT_WRITE) );
#endif
  return writeh;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
/* Double-Linked Implementation of nodes. */

#ifdef CEAL_MODREF_RING_DOUBLE_LINKED

static T(tag) F(node_tag)(T(node)* node) {
  return node->tag;
}

static T(node)* F(node_next)(T(node)* node) {
  return node->next;
}

static T(node)* F(node_prev)(T(node)* node) {
  return node->prev;
}

static void
F(node_insert)(T(modref)* modref, T(node)** next_ptr,
               T(node)* node, T(tag) tag)
{
  T(node)* next = *next_ptr;
  T(node)* prev = (T(node)*) next_ptr;

  prev->next = node;
  node->next = next;
  node->prev = prev;

  if(next)
    next->prev = node;

  node->tag = tag;
#if CEAL_DEBUG_MODTYP
  node->modtyp = CEAL_MODTYP_RING;
#endif
}

static void
F(node_remove)(T(node)* node) {
  T(node)* next = node->next;
  T(node)* prev = node->prev;
  prev->next = next;

  if(next)
    next->prev = prev;
}

static int
F(node_has_same_modref)(T(node)* node, T(modref)* modref) {
  /* We don't store the modref, so we don't know.. we could look for
     it in the linked list, but that wouldn't save us much
     time..right? */
  return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
/* Single-Linked Implementation of nodes. */

#else

static T(tag) F(node_tag)(T(node)* node) {
  return node->tag;
}

static T(node)* F(node_next)(T(node)* node) {
  return node->next;
}

static T(modref)* F(node_modref)(T(node)* node) {
  return node->modref;
}

static void
F(node_insert_)(T(modref)* modref, T(node)** next_ptr,
               T(node)* node, T(tag) tag) {
  
  T(node)* next = *next_ptr;
  *next_ptr = node;

  node->next   = next;
  node->modref = modref;

  node->tag    = tag;  
#if CEAL_DEBUG_MODTYP
  node->modtyp = CEAL_MODTYP_RING;
#endif
}

static void
F(node_remove)(T(node)* node) {
  T(node)** n_ptr = &(node->modref->nodes);
  T(node)*  n     =   node->modref->nodes;
  
  if(n) {
    while(n) {
      if(n == node) {
        /* Found it; remove it. */
        *n_ptr = n->next;
        return;
      }
      else {
        /* Keep looking. */
        n_ptr = &(n->next);
        n     = n->next;
      }
    }
    assert(!"node not found in modref's list of nodes.");
  }
  else {
    /* Modref has been zeroed
       ==> it will be reclaimed,
       no need to do removal. */
  }  
}

static void
F(node_insert)(T(modref)* modref, T(node)** next_ptr,
               T(node)* node, T(tag) tag) {
  F(node_insert_)(modref, next_ptr, node, tag);
}

static int
F(node_has_same_modref)(T(node)* node, T(modref)* modref) {
  return F(node_modref)(node) == modref;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

static Tv F(read_invoke_)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {

  T(modref)* modref = (T(modref)*) ptr;
  Tv value;
  
  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " R- handle %p, pointer %p",
       VAS_TRNODE(trnode),    readh,     ptr);

  /* Get the (only) write */
  T(writeh)* writeh = F(writeh_of_node)( modref->nodes );
  value = writeh->value;

  if( readh != NULL ) { /* ==> Core-level read. */
    /* Save readh. */
    F(node_insert)(modref, &(writeh->node.next), &(readh->node), E(EVENT_READ));
    readh->value  = value;
    readh->trnode = trnode;
  }

  /* Logging Post Conditions */
  logg(FMT_TRNODE        " R+ handle %p, pointer %p, value " Fv,
       VAS_TRNODE(trnode),    readh,     ptr,        value);

  return value;
}

Tv F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.read_invoke ++;
#endif
 
  return F(read_invoke_)(trnode, readh, ptr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

static void F(read_revoke_)(ceal_trnode_t *trnode, T(readh) *readh) {

  /* Remove it from the read list */
  F(node_remove)(&(readh->node));
}

void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh) {
      
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.read_revoke ++;
#endif

  F(read_revoke_)(trnode, readh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

Tv F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {
    
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.read_revinv ++;
#endif

  /* A tiny micro optimization that is possible here: detect if we are
     reading the same modref as before.. if so, we don't have to move
     it from one reader list to another.  This is the common case for
     read_revinv.  However, it requires storing enough state to
     perform this detection and/or spending a bit of time doing a
     conservative approximation. */
  
  if( F(node_has_same_modref)(&readh->node, (T(modref)*) ptr) ) {
    return readh->value;
  }
  else {
    F(read_revoke_)(trnode, readh);
    return F(read_invoke_)(trnode, readh, ptr);
  }
}


static void
F(initialize_writeh)(ceal_trnode_t *trnode,
                     T(writeh)* writeh,
                     T(modref)* modref,
                     Tv val)
{
  F(node_insert)(modref, &(modref->nodes), &(writeh->node), E(EVENT_WRITE));
  writeh->value = val;

#ifdef CEAL_DEBUG_MODTYP
  /* Note: normally we do not save the trnode..
     normally we do not need it. */
  writeh->trnode = trnode;
#endif
  
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

static void
F(enqueue_readers)(T(node)* node, Tv val) {
  while( node ) {
    T(readh)* readh = F(readh_of_node)( node );
    if( readh->value == val ) {
      /* Skip enqueue; the readers are consistent. */
      return;
    }
    else {
      ceal_trnode_enqueue( readh->trnode );
      readh->value = val;            
    }
    node = F(node_next)(node);
  }
}

static void
F(write_invoke_)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {  
  /* --------------------------------------------------------------------------------- */
  /* Case | Exists W? | Exists Rs? | This W? | What to do?                             */
  /* --------------------------------------------------------------------------------- */
  /* 1     No          No           Meta      Alloc + initialize writeh.               */  
  /* 2     No          No           Core      Initialize writeh                        */
  /* 3     No          Yes          Meta      --error--                                */
  /* 4     No          Yes          Core      Init. writeh; enqueue readers if needbe. */
  /* 5     Yes-Meta    ---          Meta      Reuse writeh; enqueue readers if needbe. */
  /* 6     Yes-Meta    ---          Core      --error-- mult. writes.                  */
  /* 7     Yes-Core    ---          Meta      --error-- mult. writes.                  */
  /* 8     Yes-Core    ---          Core      Init. writeh; enqueue readers if needbe. */
  /* --------------------------------------------------------------------------------- */

  T(modref)* modref = (T(modref)*) ptr;
  
  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " W- handle %p, pointer %p, value " Fv,
       VAS_TRNODE(trnode), writeh,       ptr,        val);
  
  /* Are there existing writes or reads ? */ 
  if( modref->nodes == NULL ) { /* No. */

    /* Current write? */
    if( writeh == NULL ) { /* Meta */
      /* Case 1 */
      F(initialize_writeh)( trnode,
#if 0
                            basemm_malloc(sizeof(T(writeh))),
#else
                            /* Use the metaboxes interface for allocation; This is a
                               stop-gap measure until we have a better representation of
                               modrefs, such that meta-level writes are unboxed (within the
                               modref representation itself). */
                            ceal_alloc_invoke(NULL, NULL, sizeof(T(writeh))),
#endif
                            modref, val );
    }
    else { /* Core */
      /* Case 2 */
      F(initialize_writeh)( trnode, writeh, modref, val );
    }
  }
  else { /* Yes.. */

    /* Is it (one or more) readers? */
    if( F(node_tag)( modref->nodes ) == E(EVENT_READ) ) { /* Yes. */

      if( writeh == NULL ) {
        /* Case 3 */
        assert(!"first (meta-level) write, but modref already has readers");
      }
      else {
        /* Case 4. */
        /* Update the readers' values and enqueue, if necessary. */
        F(enqueue_readers)( modref->nodes, val );
        F(initialize_writeh)( trnode, writeh, modref, val );        
      }
    }
    else if( F(node_tag)( modref->nodes ) == E(EVENT_WRITE) ) {

      /* There was a previous write. */      
      T(writeh)* prev_writeh = F(writeh_of_node)( modref->nodes );

      if( writeh == NULL ) { /* Case 5. */

#ifdef CEAL_DEBUG_MODTYP
        /* -- assert that prev_writeh is Meta-level
           --- this will exclude Case 6. */
        assert( prev_writeh->trnode == NULL );
#endif        
        prev_writeh->value = val;
        F(enqueue_readers)( F(node_next)(&prev_writeh->node), val );
      }
      else {

#ifdef CEAL_DEBUG_MODTYP
        /* Case 8. */
        /* -- assert that prev_writeh is Core-level.
           --- this will exclude Case 7. */
        
        assert( prev_writeh->trnode != NULL );

        /* For extra sanity, we also check that if the previous write
           is indeed a core-write, that it comes *later* in the
           timeline that the current write.  This is the only
           allowable possiblity (it cannot come *earlier* in the
           timeline).  It is allowable because the later write can
           (and should) be removed from the timeline before change
           propagation completes; for now we do not check for this
           removal, as it cannot be done here. */
        
        assert( totalorder_compare( ceal_trnode_time( trnode ),
                                    ceal_trnode_time( prev_writeh->trnode ) ) < 0 );
#endif
        
        F(enqueue_readers)( F(node_next)(&prev_writeh->node), val );
        F(initialize_writeh)( trnode, writeh, modref, val );
      }
    }
    else {
      abort();
    }
  }
  
  logg(FMT_TRNODE        " W+ handle %p, pointer %p, value " Fv "",
       VAS_TRNODE(trnode),    writeh,    ptr,        val);
}


void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.write_invoke ++;
#endif

  F(write_invoke_)(trnode, writeh, ptr, val);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

static void F(write_revoke_)(ceal_trnode_t *trnode, T(writeh) *writeh) {

  /* Remove it from the read list */
  F(node_remove)(&(writeh->node));
}

void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh) {

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.write_revoke ++;
#endif

  F(write_revoke_)(trnode, writeh);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */

void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.write_revinv ++;
#endif

  if( F(node_has_same_modref)(&writeh->node, (T(modref)*) ptr) ) {
    /* Logging Pre Conditions */
    logg(FMT_TRNODE        " W- handle %p, pointer %p, value " Fv,
         VAS_TRNODE(trnode), writeh,       ptr,        val);

    /* Modref is same as before-- no need to move the writeh from one
       modref to another.  Just enqueue the readers (if the written
       value has changed). */
    if(writeh->value != val) {

      F(enqueue_readers)( F(node_next)(&writeh->node), val );

      logg(FMT_TRNODE        " W+ handle %p, pointer %p, value " Fv " (changed from " Fv ")",
           VAS_TRNODE(trnode),    writeh,    ptr,        val,          writeh->value);

      /* BUG-FIX: This is crucial: need to actually update the writeh! */
      writeh->value = val;
    }
    else {
      logg(FMT_TRNODE        " W+ handle %p, pointer %p, value " Fv " (unchanged)",
           VAS_TRNODE(trnode),    writeh,    ptr,        val);
    }
  }
  else {
    F(write_revoke_)(trnode, writeh);
    F(write_invoke_)(trnode, writeh, ptr, val);
  }
}
