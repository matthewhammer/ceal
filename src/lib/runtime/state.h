/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: state.h
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

#ifndef __CEAL_STATE_H__
#define __CEAL_STATE_H__

#include "trace.h"
#include "pqueue.h"
#include "stack.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Analytic Stats.
 */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* State -- The global state of the CEAL program.
*/

typedef struct ceal_state_s {
  ceal_init_flags_t init_flags;
  
  enum { CEAL_NO_CORE ,
         CEAL_CORE_FROM_SCRATCH ,
         CEAL_CORE_COMPLETE,
         CEAL_PROPAGATION_RUNNING,
         CEAL_PROPAGATION_COMPLETE,
  } phase;
  
  /* These are fixed (after initialization):  */

  ceal_trnode_t* first;
  ceal_time_t*   time_first;
  ceal_trnode_t* last;
  ceal_time_t*   time_last;
  
  ceal_pqueue_t* pqueue;
  ceal_stack_t*  stack;

  /* The initial (root) scope.  (Aug 5 2011): It used to be that a
     core program wouldn't initially have a scope by default (the core
     program had to explicitly create one to use the memo keyword),
     but then I would always forget to create one and get an assertion
     failure (or segfault).  It seems better to always have one to
     begin with. */
  ceal_scopeh_t  root_scopeh;
  
  /* The "Dirty" Set.
     
     TEMP: Until we have proper TDTs, this set of dirty "things" helps
     us give meta-level reads the correct semantics:
     
     If x \in Dirty, then both of the following:
       1. x was changed at the meta-level, AND
       2. core-level is inconsistent.
     Meta-level reads see the changed value.
       
     Otherwise, x \not\in Dirty and either:
       1. x never changed at meta-level, OR
       2. change-propagation is running.
     Meta-level reads see the value updated via change-propagation.

    With "correct" TDTs, our priority queue subsumes the dirtyset---it
    is itself a set of inconsistent ("dirty") things.  By contrast, as
    it stands currently, the queue is a set of inconsistent _readers_
    of dirty things, and one dirty thing can have multiple enqueued
    readers.  To get the TDT semantics to work correctly, the queue's
    elements must correspond to dirty things and not their readers
    (the next dirty reader is always known by any dirty thing in
    TDTs).
  */
  ceal_stack_t*  dirtyset_meta;

  /* The Core-level dirty set is similar to the Meta-level dirty set,
     except that it (1) grows during core invocation and
     revocation--not during meta-level execution, where it is empty,
     and (2) we use it for a different purpose: to postpone
     feedback---where (optionally) the final state of the core program
     is reflected back as its next initial state. */
  ceal_stack_t*  dirtyset_core;
  
  /* These change: */  
  ceal_time_t*   time_now;
  ceal_time_t*   time_redo_end;
  ceal_scope_t   scope;

  /* Two types of garbage: */
  struct {
    uintptr_t           autogc_toggle;
    uintptr_t           collections;
    ceal_trnode_garb_t* trnodes;
    ceal_box_t*         boxes;
  } garbage;

  /* A list of metaboxes.  We append into this list for each
     allocation performed at the meta level.  */
  ceal_alloch_t* metaboxes;
  
#ifdef CEAL_ANALYTIC_STATS
  /* List of descriptors */
  ceal_desc_t* descs;
  
  /* Analytic stats */
  struct ceal_analytic_stats_s {
    long propagations;
    
    long trnode_new;
    long trnode_undo;
    long trnode_free;
    long trnode_redo;
    
    long read_invoke;
    long read_revinv;
    long read_revoke;
    
    long write_invoke;
    long write_revinv;
    long write_revoke;
    
    long memo_invoke;
    long memo_revoke;

    /* -- Hashtable operations -- */

    long memo_hit;

    long memo_miss;
    
    long memo_move;   /* # of times any entry is moved / copied into a
                           bigger/smaller table  */
    
    long memo_search; /* # of times any bucket is compared against a
                           search target */
    
  } analytic_stats__from_scratch,
    analytic_stats;
#else
  /*#warning CEAL RT will not collect analytic stats*/
#endif
  
} ceal_state_t;

extern ceal_state_t* ceal_state;

#endif

