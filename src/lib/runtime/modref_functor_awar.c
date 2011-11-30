/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_functor_awar.c
Authors:
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

/*
 * Matthew Hammer <hammer@ttic.edu>
 * Yan Chen       <chenyan@ttic.edu>
 */

#include <stdlib.h>
#include <assert.h>

#include "basemm.h"
#include "state.h"
#include "logging.h"

/* = = = = = = = = = = = = = = = = */
/* Representation-specific details */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* -- Getters and Setters for the modref. */

/* Get the two LSBs */
static uintptr_t F(lsbs_of_word) (uintptr_t bits) {
  return bits & ((uintptr_t) 0x3);
}

/* Get the LSBs */
static uintptr_t F(lsbs_of_modref) (T(modref)* modref) {
  return F(lsbs_of_word)(modref->u.bits);
}

/* Get the events */
static T(event)* F(events_of_modref) (T(modref)* modref) {
  /* Return a pointer (mask the two LSBs) */
  return (T(event)*) ((modref->u.bits) & (~((uintptr_t) 0x3)));
}

/* Set the events */
static void F(modref_set_events) (T(modref)* modref, T(event)* root) {
  uintptr_t a = F(lsbs_of_modref)(modref);
  uintptr_t b = (uintptr_t) root;
#if CEAL_DEBUG
  assert(F(lsbs_of_word)(b) == 0);
#endif
  modref->u.bits = a | b;
}

/* Set the LSBs */
static void F(modref_set_lsbs) (T(modref)* modref, uintptr_t bits) {
  uintptr_t a = (uintptr_t) F(events_of_modref)(modref);
#if CEAL_DEBUG
  assert( F(lsbs_of_word)(bits) == bits );
#endif
  modref->u.bits = a | bits;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* -- ignoring the splay-tree components (left and right pointers),
      the following helper functions provide getters/setters for the
      remaining event structure. */

static T(event)* F(event_set_trnode)
     (T(event)* event, ceal_trnode_t* trnode)
{
#ifdef CEAL_DEBUG
  assert( event != NULL );
#endif
  event->trnode = trnode;

#ifdef CEAL_DEBUG_MODTYP
  event->modtyp = CEAL_MODTYP_AWAR;
#endif

  return event;
}


static void F(event_init)
     (T(event)* t,
      ceal_trnode_t* trnode,
      uintptr_t tag,
      T(modref)* modref,
      Tv value)
{
  t->trnode = trnode;
  t->tag    = tag;
  t->modref = modref;
  t->value  = value;

#ifdef CEAL_DEBUG_MODTYP
  t->modtyp = CEAL_MODTYP_AWAR;
#endif
}

static uintptr_t F(tag_of_event) (T(event)* t) {
  /* return (t->u.event_tag & 3); */
  return t->tag;
}

static ceal_trnode_t* F(trnode_of_event)(T(event)* t) {
  /*
    return (ceal_trnode_t*)
    ((uintptr_t)t->u.trnode & (~((uintptr_t)3)));
  */
  return t->trnode;
}

static T(modref)* F(modref_of_event)(T(event)* t) {
  /*
    return (ceal_trnode_t*)
    ((uintptr_t)t->u.trnode & (~((uintptr_t)3)));
  */
  return t->modref;
}

/* Compare two events:

   -1 means t1 occurs before t2
    0 means t1 and t2 occur together (invariant: t1 == t2)
   +1 means t1 occurs after t2

   Note: if either are NULL, they are treated as occuring "now".   
*/
static int F(event_comp)(T(event)* t1, T(event)* t2) {

  ceal_time_t* time1 = ( t1
                         ? ceal_trnode_time(F(trnode_of_event)(t1))
                         : ceal_state->time_now );

  ceal_time_t* time2 = ( t2
                         ? ceal_trnode_time(F(trnode_of_event)(t2))
                         : ceal_state->time_now );

#ifdef CEAL_DEBUG
  /* Invariant: distinct trace nodes have distinct times */
  if(t1 && t2)
    if(F(trnode_of_event)(t1) != F(trnode_of_event)(t2))
      assert(time1 != time2);  
#endif
  
  if(time1 == time2) {
#ifdef CEAL_DEBUG
    /* Invariant: if times are equal and one event is null, then they
       are both null.  This invariant justifies the subtraction step
       we do next.*/
    if(t1 == NULL || t2 == NULL) {
      assert(t1 == NULL && t2 == NULL);
    }
#endif
    /* BUG-FIX: returning simply (t1 - t2) doesn't do the right thing.
       Even if you are careful to cast these pointers to either void*
       or char*.  */
    
    if( t1 == t2 ) {
      return 0;
    }
    else if( ((uintptr_t) t1) < ((uintptr_t) t2) ) {
      return -1;
    }
    else if( ((uintptr_t) t1) > ((uintptr_t) t2) ) {
      return 1;
    }
    else {
      abort();
    }
  }
  else {
    return totalorder_compare(time1, time2);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Splay -- Simple top down splay, not requiring new_root to be in the
   tree.  If new_root is not in the tree, return the end position of
   binary search.  If new_root is NULL, then we search for "now". */

static T(event)* F(event_splay) (T(event) *root, T(event) *new_root) {

  /* logg("event_splay(%p, %p)", root, new_root); */
  
#ifdef CEAL_DEBUG_MODTYP
  if(root) {
    assert( root->modtyp == CEAL_MODTYP_AWAR );
  }
  assert( new_root->modtyp == CEAL_MODTYP_AWAR );
#endif
  
  if (root == NULL)
    return NULL;  
 
  else if(root == new_root)
    return root;
  
  else {

    T(event) N = {
#ifdef CEAL_DEBUG_MODTYP
      CEAL_MODTYP_JUNK,
#endif
      NULL, NULL};
    
    T(event) *l = &N;
    T(event) *r = &N;
    T(event) *t = root;    
    
    for (;;) {
      int comp = F(event_comp)(new_root, t);

      if(comp == 0)
        break;
      
      else if(comp < 0) {
        /* new root to left of t. */
        
        if (t->left == NULL)
          break;
        
        if (F(event_comp)(new_root, t->left) < 0) {

          /* rotate right */
          T(event)* y = t->left;
          t->left = y->right;
          y->right = t;
          t = y;
          if (t->left == NULL)
            break;
        }

        /* link right */
        r->left = t;
        r = t;
        t = t->left;
        
      }
      else if (comp > 0) {
        /* new root to right of t. */
        
        if (t->right == NULL)
          break;
        
        if (F(event_comp)(t->right, new_root) < 0) { 

          /* rotate left */
          T(event)* y = t->right;
          t->right = y->left;
          y->left = t;
          t = y;

          if (t->right == NULL)
            break;
        }

        /* link left */
        l->right = t;
        l = t;
        t = t->right;
      }      
      else
        abort();
    }

    /* assemble */
    l->right = t->left;
    r->left = t->right;
    t->left = N.right;
    t->right = N.left;

#ifdef CEAL_DEBUG
    assert( t != NULL );
#endif
    
    return t;
  }
}
                            
/* Insert new_event into the tree t. */
/* Return a pointer to the resulting tree. */
static T(event)* F(event_insert)(T(event)* t, T(event)* new_event) {

  /* logg("event_insert(%p, %p)", t, new_event); */
  
  if (t == NULL) {
    new_event->left  = NULL;
    new_event->right = NULL;
    return new_event;
  }
  
  t = F(event_splay)(t, new_event);
  
  if (F(event_comp)(new_event, t) < 0) {
    new_event->left = t->left;
    new_event->right = t;
    t->left = NULL;
    return new_event;
  }
  else if (F(event_comp)(t, new_event) < 0) {
    new_event->right = t->right;
    new_event->left = t;
    t->right = NULL;
    return new_event;
  }
  else
    abort();
}


/* Removes old_event from the tree if it's there.  */
/* Return a pointer to the resulting tree. */
static T(event)* F(event_remove)(T(event) *t, T(event)* old_event) {
  if (t == NULL) {
    return NULL;
  }
  else {

    t = F(event_splay)(t, old_event);

    if(t == old_event) {
      
      if (t->left == NULL) {
        return t->right;
      }
      
      else {
        T(event) *x = F(event_splay)(t->left, old_event);
        x->right = t->right;
        return x;
      }

    }
    else  /* It wasn't there */
      abort();
  }
}


/* Return previous event, or t if no such event exists */
static T(event)* F(event_prev)(T(event)* t) {

  /* logg("event_prev(%p)", t); */
  
  if (t == NULL)
    return t;

  else if (t->left == NULL) {
    return t;
  }
  
  else {
    t->left = F(event_splay)(t->left, t);
    return F(event_splay)(t, t->left);
  }
}

/* Return next event, or t if no such event exists. */
static T(event)* F(event_next)(T(event)* t) {

  /* logg("event_next(%p)", t); */
  
  if (t == NULL)
    return NULL;

  else if(t->right == NULL) {
    return t;
  }
  
  else {
    t->right = F(event_splay)(t->right, t);      
    return F(event_splay)(t, t->right);
  }
}


/* Find the greatest event that occurs before the given one.
   The given one need not exist in the tree. */
/* In the case when no predecessor exists, returns the first event in the tree. */
/* This operation is used during the invocation of both reads and writes */
static T(event)* F(event_pred_of)(T(event)* t, T(event)* succ) {

  /* logg("event_pred_of(%p, %p)", t, succ); */
  
  if(t == NULL)
    return NULL;

  else {
    t = F(event_splay)(t, succ);
    
    if(t == succ ||
       F(event_comp)(succ, t) < 0) {
      return F(event_prev)(t);
    }
    else {
      return t;
    }
  }
}


/* Find the least event that occurs after the given one.
   The given one need not exist in the tree. */
/* In the case when no successor exists, returns the last event in the tree. */
/* This operation is used during the revocation of writes */
static T(event)* F(event_succ_of)(T(event)* t, T(event)* pred) {

  /* logg("event_succ_of(%p, %p)", t, pred); */
  
  if(t == NULL)
    return NULL;

  else {
    t = F(event_splay)(t, pred);
    
    if(t == pred ||
       F(event_comp)(t, pred) < 0)
      return F(event_next)(t);
    else
      return t;
  }
}



/* Enqueue all the reads that occur at t and afterwards, up until the
   first write occuring after t. */
static T(event)* F(event_enqueue_reads)(ceal_trnode_t* trnode, T(event)* t)
{

  while (t && (F(tag_of_event)(t) == E(EVENT_READ))) {    
    /* Does the successor occur (strictly) after this write? */
    if(trnode != F(trnode_of_event)(t)) {
      /* If so, there's no need to enqueue these reads--
         we will necessarily evaluate them
         (by virtue of them being in the same trace node). */
      ceal_trnode_enqueue(F(trnode_of_event)(t));
    }

    if(t->right)
      t = F(event_next)(t);
    else
      break;
  }
  return t;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Getting/Setting/Clearing dirty status */

typedef enum {
  E(__CLEAR)  = 0x0,
  E(__UNUSED) = 0x1,
  E(DIRTY)    = 0x2,
} T(bits);

uintptr_t F(modref_is_dirty)(T(modref)* m) {
  return F(lsbs_of_modref)(m) & E(DIRTY);
}

static void F(modref_set_dirty)(T(modref)* m) {
  F(modref_set_lsbs)(m, E(DIRTY));
}

static void F(modref_clear_dirty_)(T(modref)* m) {
#if CEAL_DEBUG
  assert( F(modref_is_dirty)(m) );
#endif
  F(modref_set_lsbs)(m, E(__CLEAR));
}

static void F(modref_clear_dirty)(void* m) {
  F(modref_clear_dirty_)( (T(modref)*) m );
}

static T(event)* F(meta_write_of_events)(T(event)* t) {
  
  /* Use this dummy event to search for first event */
  T(event) first = {
#ifdef CEAL_DEBUG_MODTYP
    CEAL_MODTYP_AWAR,
#endif
    NULL, NULL, ceal_state->first
  };
  
  /* Get the first event */
  t = F(event_pred_of)(t, & first);
  
#ifdef CEAL_DEBUG
  /* If it exists, it had better occur first */
  assert( t == NULL || F(trnode_of_event)(t) == ceal_state->first );
#endif

  return t;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Read interface */


static Tv F(read_invoke_)(ceal_trnode_t *trnode, T(readh) *readh, Tv *pointer) {

  /* The pointer actually points at a modref */
  T(modref)* modref = (T(modref)*) pointer;
  T(event)* t = F(events_of_modref)(modref);
  T(event)* pred = NULL;
  Tv value;

  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " R- handle %p, pointer %p",
       VAS_TRNODE(trnode),    readh,     pointer);
  
  if( readh == NULL ) { /* ==> Meta-level read. */

    /* Is the modref dirty, or is it clean? */
    if( F(modref_is_dirty)(modref) ) {
      /* Semantic Reasoning: The modref is dirty; So, the core
         computation is inconsistent.  Hence, we should return the
         most recent write value done at the meta-level (we do not
         store previous meta-level write values anyway).  We know it
         exists since otherwise the modref would not be dirty. */
      T(event)* meta_write = t = F(meta_write_of_events)( t );      
      value = meta_write->value;
    }
    else {
      /* Semantic Reasoning: The modref is not dirty; So, since we are
         reading at the meta-level, it must be consistent with the
         core computation.  Hence, we find the last write (either at
         meta or core) and return its value. */
      
      /* Use this dummy event to search for last event */
      T(event) last = {
#ifdef CEAL_DEBUG_MODTYP
        CEAL_MODTYP_AWAR,
#endif
        NULL, NULL, ceal_state->last
      };

      /* Get the predecessor (relative to the given read handle)
         It has the current value. */
      /* NULL-valued read handles request the value "now". */
      T(event)* succ = t = F(event_succ_of)(t, & last);
      
#ifdef CEAL_DEBUG
      if(succ == NULL)
        assert(!"meta-level read_invoke on empty modref");
#endif

      value = succ->value;
    }
  }
  else { /* ==> Core-level read. */
    
    /* Get the predecessor (relative to the given read handle)
       It has the current value. */
    pred = t =
      F(event_pred_of)(t, F(event_set_trnode)(readh, trnode));
    
#ifdef CEAL_DEBUG

    if(pred == NULL)
      assert(!"core-level read_invoke on empty modref");

    /* TODO */
           
#endif
    
    /* Insert the read handle as a splay tree node */ 
    t = F(event_insert)(t, readh);
    
    /* Initialize the event */
    F(event_init)
      (readh,
       trnode,
       E(EVENT_READ),
       modref,
       pred->value);

    value = pred->value;

  }

  /* Logging Post Conditions */
  logg(FMT_TRNODE        " R+ handle %p, pointer %p, pred %p, value " Fv,
       VAS_TRNODE(trnode),    readh,     pointer,    pred,    value);


#ifdef CEAL_DEBUG
  assert( t );
#endif
  
  /* Save the updated events */
  F(modref_set_events)(modref, t);

  return value;
}

Tv F(read_invoke)(ceal_trnode_t *trnode, T(readh) *readh, Tv *pointer) {
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.read_invoke ++;
#endif
  
  return F(read_invoke_)(trnode, readh, pointer);
}

static void F(read_revoke_)(ceal_trnode_t *trnode, T(readh) *readh) {
  T(modref)* modref = F(modref_of_event)(readh);

  logg(FMT_TRNODE        " r- handle %p, pointer %p",
       VAS_TRNODE(trnode),    readh,     modref);

  if( ! F(events_of_modref)(modref) ) {
    /* ==> NULL splay tree
       ==> The modref has already been reclaimed. */
    return;
  }
  else {    
    F(modref_set_events)
      (modref, F(event_remove)
       ( F(events_of_modref)(modref), readh) );
  }

  logg(FMT_TRNODE        " r+ handle %p, pointer %p",
       VAS_TRNODE(trnode),    readh,     modref);
}

void F(read_revoke)(ceal_trnode_t *trnode, T(readh) *readh) {
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.read_revoke ++;
#endif
  
  F(read_revoke_)(trnode, readh);
}

Tv F(read_revinv)(ceal_trnode_t *trnode, T(readh) *readh, Tv *ptr) {
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.read_revinv ++;
#endif

  F(read_revoke_)(trnode, readh);
  return F(read_invoke_)(trnode, readh, ptr);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Write interface */


static void F(write_invoke_)(ceal_trnode_t *trnode, T(writeh) *writeh,
                             Tv *pointer, Tv value) {
  
  /* The pointer actually points at a modref */
  T(modref)* modref = (T(modref)*) pointer;
  T(event)*       t = F(events_of_modref)(modref);
  T(event)*    pred = NULL;  
  Tv     pred_value;
  
  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " W- handle %p, pointer %p, value " Fv,
       VAS_TRNODE(trnode), writeh,       pointer,    value);
  
  if(writeh == NULL) { /* ==> Meta-level write. */

    /* Get the meta-level write.
       If NULL, we know that no other events exist yet. */
    pred = t = F(meta_write_of_events)( t );
    
    /* Do we already have a meta-level write event? */
    if( pred ) { /* Yes. */

      if( ceal_state->phase != CEAL_NO_CORE ) {
        /* Does new value match old value?
           If not, enqueue all affected readers. */
        if(pred->value != value) {

          /* Is the first dirty meta-level write since the last
             change-propagation? */
          if( ! F(modref_is_dirty)(modref) ) {

            /* Enqueue all the readers. */
            t = F(event_enqueue_reads)(trnode, F(event_next)(pred));

            /* Set dirty; add it to the dirtyset. */
            F(modref_set_dirty)(modref);
            ceal_dirtyset_add(modref, F(modref_clear_dirty));
          }
        }
      }
      
      /* Store the new value */
      pred_value  = pred->value;
      pred->value = value;
      
    }
    else { /* No. So make one.*/

#ifdef CEAL_DEBUG
      /* Sanity Check: If we are doing a meta-level write, and if no
         previous meta-level write exists, then no other events exist
         at all. */
      assert( F(events_of_modref)(modref) == NULL );
#endif
      
      T(event)* meta_write =
#if 0
        basemm_malloc(sizeof(T(event)));
#else
      /* Use the metaboxes interface for allocation; This is a
         stop-gap measure until we have a better representation of
         modrefs, such that meta-level writes are unboxed (within the
         modref representation itself). */
      writeh = ceal_alloc_invoke(NULL, NULL, sizeof(T(writeh)));
#endif

      /* Initialize the event. */
      F(event_init)
        (meta_write,
         ceal_state->first,
         E(EVENT_WRITE),
         modref,
         value);
      
      /* Insert the event. */ 
      t = F(event_insert)(t, meta_write);
    }
  }
  else { /* ==> Core-level write. */
    
    /* Get the predecessor (relative to the given read handle)
       It has the current value. */
    pred = t = F(event_pred_of)(t, F(event_set_trnode)(writeh, trnode));
    
    /* Insert the event. */ 
    t = F(event_insert)(t, writeh);
    
    /* Does new value match old value?
       If not, enqueue all affected readers. */
    if(pred) {
    
      pred_value = pred->value;
      
      if( pred->value != value ) {
      
        /* Get the sucessor of this write, If any. */
        t = F(event_succ_of)(t, writeh);

        /* logg("pred %p, writeh %p, succ %p", pred, writeh, t); */        
        t = F(event_enqueue_reads)(F(trnode_of_event)(writeh), t);
      }
    }
    
    /* Initialize the event */
    F(event_init)
      (writeh,
       trnode,
       E(EVENT_WRITE),
       modref,
       value);
  }
    
  /* Logging Post Conditions */
  if( pred ) {
    logg(FMT_TRNODE        " W+ handle %p, pointer %p, value " Fv " (was " Fv ", pred %p)",
         VAS_TRNODE(trnode),    writeh,    pointer,    value,        pred_value, pred);
  }
  else {
    logg(FMT_TRNODE        " W+ handle %p, pointer %p, value " Fv " (initial)",
         VAS_TRNODE(trnode),    writeh,    pointer,    value);
  }
  
  /* Save the updated events */
#ifdef CEAL_DEBUG
  assert( t );
#endif
  F(modref_set_events)(modref, t);
}

void F(write_invoke)(ceal_trnode_t *trnode, T(writeh) *writeh,
                     Tv *pointer, Tv value) {

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.write_invoke ++;
#endif

  F(write_invoke_)(trnode, writeh, pointer, value);
}

static void F(write_revoke_)(ceal_trnode_t *trnode, T(writeh) *writeh) {
  
  T(modref)* modref = (T(modref)*) F(modref_of_event)(writeh);

  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " w- handle %p, pointer %p",
       VAS_TRNODE(trnode), writeh,       modref);
  
#if CEAL_DEBUG
  assert(writeh);
#endif

  if( ! F(events_of_modref)(modref) ) {
    /* ==> NULL splay tree
       ==> The modref has already been reclaimed. */
    return;
  }
  else {
    T(event)* t = F(events_of_modref)(modref);
    T(event)* pred = t = F(event_pred_of)(t, writeh);

    /* Case 1: If the predecessor is identical to writeh, we can assume
       that writeh is the very first write (and we must assume,
       conservatively, that all reads that follow it will be affected).
       
       Case 2: If previous value is diff then the revoked one, Enqueue
       all the readers that occur after the revoked write, since now
       they see the (different) previous value.
       
       Case 3: Otherwise, we need not enqueue any readers.
    */
    if( /* Case 1: */ (pred == writeh) ||
        /* Case 2: */ (pred->value != writeh->value) ) {
      T(event)* succ = F(event_succ_of)(t, writeh);      
      t = F(event_enqueue_reads)(trnode, succ);
    }    

    /* Remove the event. */
    F(modref_set_events)( modref, F(event_remove)(t, writeh) );
  }

  /* Logging Pre Conditions */
  logg(FMT_TRNODE        " w+ handle %p, pointer %p",
       VAS_TRNODE(trnode), writeh,       modref);
}

void F(write_revoke)(ceal_trnode_t *trnode, T(writeh) *writeh) {

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.write_revoke ++;
#endif

  F(write_revoke_)(trnode, writeh);
}


void F(write_revinv)(ceal_trnode_t *trnode, T(writeh) *writeh, Tv *ptr, Tv val) {

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.write_revinv ++;
#endif
  
  F(write_revoke_)(trnode, writeh);
  F(write_invoke_)(trnode, writeh, ptr, val);
}

