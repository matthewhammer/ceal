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

/* Matthew Hammer <hammer@mpi-sws.org> */

#include <err.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "logging.h"
#include "basemm.h"
#include "hash.h"
#include "trace.h"
#include "state.h"
#include "tv_signal.h"

#define MINUS_SEP_08 "- - - - - "  "- - - "
#define MINUS_SEP_10 "- - - - - "  "- - - - - "  
#define MINUS_SEP_20 MINUS_SEP_10  MINUS_SEP_10
#define MINUS_SEP_80 MINUS_SEP_20  MINUS_SEP_20  MINUS_SEP_20  MINUS_SEP_20

#define PLUS_SEP_08 "+ + + + + "  "+ + + "
#define PLUS_SEP_10 "+ + + + + "  "+ + + + + "
#define PLUS_SEP_20 PLUS_SEP_10   PLUS_SEP_10
#define PLUS_SEP_80 PLUS_SEP_20   PLUS_SEP_20   PLUS_SEP_20   PLUS_SEP_20

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Trace nodes */

static ceal_desc_t*
ceal_trnode_desc(ceal_trnode_t* trnode) {
  /* We use the low bits of .desc to track whether a trace node is
     enqueued or not. */
  return (ceal_desc_t*) (((uintptr_t) trnode->desc) & ~((uintptr_t) 1));
}

static ceal_desc_stats_t*
ceal_desc_stats_by_phase(ceal_desc_t* desc) {
  static ceal_desc_stats_t dummy;

  if ( desc->stats_fromscratch == NULL ||
       desc->stats_propagation == NULL ) {
    return &dummy;
  }       
  if( ceal_state->phase == CEAL_CORE_FROM_SCRATCH ) {
    return desc->stats_fromscratch;
  }
  else if ( ceal_state->phase == CEAL_PROPAGATION_RUNNING ||
            ceal_state->phase == CEAL_PROPAGATION_COMPLETE ) {
    return desc->stats_propagation;
  }
  else {
    abort();
    return NULL;
  }
}

int ceal_trnode_is_enqueued(ceal_trnode_t* trnode) {
  /* We use the low bits of .desc to track whether a trace node is
     enqueued or not. */
  return (int) (((uintptr_t) trnode->desc) & 1);
}

void ceal_trnode_enqueue(ceal_trnode_t* trnode) {
  logg(FMT_TRNODE " --enqueue--> Q", VAS_TRNODE(trnode));

#if CEAL_DEBUG
  if( ceal_state->phase == CEAL_CORE_COMPLETE ||
      ceal_state->phase == CEAL_PROPAGATION_COMPLETE ) {
    /* Okay. */
    logg("Not running/propagating core; hence, "FMT_TRNODE " is in 'the future'",
         VAS_TRNODE(trnode));
  }
  else if ( totalorder_compare ( ceal_state->time_now,
                                 ceal_trnode_time( trnode ) ) < 0) {
    /* Okay. */
    logg(FMT_TRNODE " is in 'the future'.  Okay to enqueue.",
         VAS_TRNODE(trnode));
  }
  else {
    /* Error. */
    logg(FMT_TRNODE " is *not* in 'the future'!", VAS_TRNODE(trnode));
    assert(!"trace node should be in the future, "
           "not in the past and/or present");
  }
#endif
  
  if( ! ceal_trnode_is_enqueued( trnode )) {
    /* We use the low bits of .desc to track whether a trace node is
       enqueued or not. */
    trnode->desc = (ceal_desc_t*) ((uintptr_t) trnode->desc | 1);
    ceal_pqueue_push( ceal_state->pqueue, trnode );
    ceal_tvsig_enqueued( trnode );
  }
  else {
    logg(FMT_TRNODE " is already in Q.", VAS_TRNODE(trnode));
  }
}

void ceal_trnode_enqueue_if_in_future(ceal_trnode_t* trnode) {

  if ( totalorder_compare ( ceal_state->time_now,
                            ceal_trnode_time( trnode ) ) < 0) {
    /* Enqueue. */
    logg(FMT_TRNODE " is in 'the future'.  Okay to enqueue.",
         VAS_TRNODE(trnode));
    ceal_trnode_enqueue( trnode );
  }
  else {
    /* Don't enqueue; but not an error. */
    logg(FMT_TRNODE " is not in 'the future'; Skipping enqueue.",
         VAS_TRNODE(trnode));
  }
}

ceal_trnode_t* ceal_trnode_dequeue() {
  ceal_trnode_t* trnode = ceal_pqueue_pop( ceal_state->pqueue );
  /* We use the low bits of .desc to track whether a trace node is
     enqueued or not. */
  trnode->desc = ceal_trnode_desc(trnode);
  ceal_tvsig_dequeued( trnode );
  logg(FMT_TRNODE " <--dequeue-- Q", VAS_TRNODE(trnode));
  return trnode;
}

static void ceal_trnode_insert(ceal_trnode_t* trnode) {

  ceal_desc_t* desc = ceal_trnode_desc(trnode);

#ifdef CEAL_DEBUG
  assert( desc->has_start_time );
#endif
  
  /* Assign the start time and advance time. */
  ceal_state->time_now = 
    totalorder_insert_succ(ceal_state->time_now,
                             & trnode->start_time);

  logg(FMT_TRNODE, VAS_TRNODE(trnode));
  
  if( desc->saves_scope ) {
    /* Save the current scope */
    trnode->scope = ceal_state->scope;

    logg(FMT_TRNODE  " saved scope %p",
         VAS_TRNODE(trnode), ceal_state->scope);

    if( desc->has_end_time ) {
      /* Push onto the stack for later. */
      ceal_stack_push( ceal_state->stack, trnode );

      logg(FMT_TRNODE " pushed", VAS_TRNODE(trnode));
      
#ifdef CEAL_DEBUG
      trnode->end_time = NULL;
#endif      
    }
  }
  else {
#ifdef CEAL_DEBUG
    assert ( ! desc->has_end_time );
#endif
  }
}

ceal_trnode_t* ceal_trnode_new(ceal_desc_t* desc) {

  /* Allocate the trace node. Store the descriptor. */
  ceal_trnode_t* trnode = basemm_malloc(desc->size);
  trnode->desc = desc;

  logg(FMT_TRNODE " "      FMT_DESC,
       VAS_TRNODE(trnode), VAS_DESC(desc));
  
  /* If no memo entry, insert trace node into trace. */
  /* Otherwise, we defer this until a memo miss. */
  if(! desc->has_memo_entry)
    ceal_trnode_insert(trnode);
    
#ifdef CEAL_ANALYTIC_STATS
  if(desc->next == NULL && desc != ceal_state->descs) {

    /* Descriptor isn't listed, so put it into the list. */
    desc->next = ceal_state->descs;
    ceal_state->descs = desc;

    /* Also, its statistics structs aren't zeroed, so do that too. */
    if(desc->stats_fromscratch)
      memset(desc->stats_fromscratch, 0, sizeof(ceal_desc_stats_t));
    if(desc->stats_propagation)
      memset(desc->stats_fromscratch, 0, sizeof(ceal_desc_stats_t));
  }
  
  ceal_desc_stats_by_phase(ceal_trnode_desc(trnode))->new ++;
  ceal_state->analytic_stats.trnode_new ++;
#endif

  ceal_tvsig_allocated( trnode, desc->debuginfo, desc->size );
  
  return trnode;
}

ceal_time_t* ceal_trnode_time(ceal_trnode_t* trnode) {
#ifdef CEAL_DEBUG
  assert(ceal_trnode_desc(trnode)->has_start_time);
#endif
  return &(trnode->start_time);
}

static ceal_trnode_t*
ceal_trnode_of_time(ceal_time_t* time) {

  /* Disgusting. */
  return (ceal_trnode_t*)
    & ( ((ceal_desc_t**) time) [-1] );
  
}

ceal_scope_t ceal_trnode_scope(ceal_trnode_t* trnode) {
#ifdef CEAL_DEBUG
  assert(ceal_trnode_desc(trnode)->saves_scope);
#endif
  return trnode->scope;
}


int ceal_trnode_compare(ceal_trnode_t* trnode1,
                        ceal_trnode_t* trnode2) {
  return totalorder_compare(&(trnode1->start_time),
                            &(trnode2->start_time));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Global state. */

ceal_state_t* ceal_state = NULL;

void ceal_init( ceal_init_flags_t init_flags ) {

  if( ceal_state == NULL ) {
    static ceal_state_t  s;
    static ceal_pqueue_t pqueue;
    static ceal_stack_t  stack;
    static ceal_stack_t  dirtyset;
    
    /* Desc for the first trace node  */
    static ceal_desc_t desc =
      CEAL_DESC_DUMMY(sizeof(ceal_desc_t*) + sizeof(ceal_time_t));
    
    basemm_init();
    ceal_logg_init();

    /* Initialization of tv signals now occurs within the compiler's
       output, depending on whether the corresponding compiler flag is
       set or not. */

    if ( init_flags & CEAL_INIT_TVSIG ) {
      FILE* f = fopen("ceal.tv", "w");
      ceal_tvsig_init( f );
    }
    else {
      ceal_tvsig_init( NULL );
    }
    
    ceal_state   = &s;

    /* Store the initialization flags. */
    s.init_flags = init_flags;
    
    /* Setup the time line. */
    s.time_now   = NULL;    
    s.first      = ceal_trnode_new( & desc );    
    s.time_first = ceal_trnode_time( s.first );
    s.last       = ceal_trnode_new( & desc );
    s.time_last  = ceal_trnode_time( s.last );
    
    /* Setup other initial conditions */
    s.time_now       = s.time_first;
    s.time_redo_end  = NULL;
    s.scope          = ceal_scope_invoke( s.first, & s.root_scopeh );
    
    /* Setup the queue, stack and dirtyset. */
    ceal_pqueue_init ( s.pqueue   = &pqueue );
    ceal_stack_init  ( s.dirtyset = &dirtyset );
    ceal_stack_init  ( s.stack    = &stack );
       
    /* Garbage lists are initially empty. */
    s.garbage.collections  = 0;
    s.garbage.trnodes      = NULL;
    s.garbage.boxes        = NULL;

    /* By default, no metaboxes allocation handle. */
    s.metaboxes            = NULL;
    
#ifdef CEAL_ANALYTIC_STATS
    /* We want to make a simple linked list of trace node descriptors.
       
       Insertion: We insert a descriptor D into the linked list
       whenever we invoke a trace node for D and (D->next == NULL).
       We use the "dummy" descriptor above as a sentinel node for the
       end of the list. */
    s.descs = &desc;
    
    /* Zero out stats */
    memset(&s.analytic_stats, 0,
           sizeof(struct ceal_analytic_stats_s));
#endif
  }
  else {
    warn("ceal_init called more than once.");
  }
  
}

ceal_init_flags_t ceal_init_flags() {
  if( ceal_state == NULL )
    return 0;
  else {
    return ceal_state->init_flags;
  }  
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Dirty sets. -- See state.h */

void ceal_dirtyset_add(void* thing, void(*cb)(void* thing)) {
  ceal_stack_push(ceal_state->dirtyset, thing);
  ceal_stack_push(ceal_state->dirtyset, cb);
}

void ceal_dirtyset_clear() {
  ceal_stack_t* dirtyset = ceal_state->dirtyset;
  
  while( ! ceal_stack_isempty( dirtyset ) ) {
    void (*cb)(void* _) = ceal_stack_pop( dirtyset );
    void* thing         = ceal_stack_pop( dirtyset );
    cb(thing);
  }
}

void ceal_dirtyset_assert_clear() {
#if CEAL_DEBUG
  assert( ceal_stack_isempty( ceal_state->dirtyset ) );
#endif
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Analytic Statistics. */

#ifdef CEAL_ANALYTIC_STATS
void ceal_save_analytic_stats() {

  int do_logg = 
#ifdef CEAL_LOGGING
    1
#else
    0
#endif
    ;

  int do_data_entry =
#ifdef CEAL_LOGGING
    1
#elif CEAL_ANALYTIC_DUMP
    ( ceal_state->analytic_stats.propagations > 0 &&
      ceal_state->analytic_stats.propagations % 1000 == 0 )
#else
    0
#endif
    ;
  
  if(do_logg || do_data_entry) {
    
    struct entry_s { const char* name; long* val; };

    struct ceal_analytic_stats_s* stats0 =
      &ceal_state->analytic_stats__from_scratch;

    assert ( stats0->trnode_free == 0 );
    
    struct ceal_analytic_stats_s* stats =
      &ceal_state->analytic_stats;
    
    struct entry_s entries[] = {
      { "propagations",  & stats->propagations },
      { "trnode_initial",& stats0->trnode_new },
      { "trnode_new",    & stats->trnode_new },
      { "trnode_undo",   & stats->trnode_undo },
      { "trnode_free",   & stats->trnode_free },
      { "trnode_redo",   & stats->trnode_redo },   
    };
    
    int entries_count = sizeof(entries) / sizeof(struct entry_s);
    
    { int i;

      if(do_logg) {
        /* Append to the log. */
        for(i = 0; i < entries_count; i++) {      
          logg("%-16s = %ld", entries[i].name, *entries[i].val);
        }
      }
      
      if(do_data_entry) { /* Append to the data file. */
        static int first_entry = 1;
        static FILE* stats_out = NULL;
        
        /* Open the stats file, if we haven't already. */
        if(!stats_out)
          stats_out = fopen("ceal-analytic-stats.data", "w+");
        
        /* buffer it only by line. */
        setvbuf(stats_out, NULL, _IOLBF, 0);
        
        /* Create the data file header? */
        if(first_entry) {
          
          fprintf(stats_out, "# ");
          for(i = 0; i < entries_count; i++) {
            fprintf(stats_out, "%d:%s ", i+1, entries[i].name);
          }
          fprintf(stats_out, "\n");        
          first_entry = 0;
        }
        
        for(i = 0; i < entries_count; i++) {
          fprintf(stats_out, "%ld ", *entries[i].val);
        }
        fprintf(stats_out, "\n");
        
      }
    }
  }
}
#endif  



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Core -- Functions for marking the begin / end of core trace     */

void ceal_core_begin() {

  logg(MINUS_SEP_20 "Core " MINUS_SEP_20);
  
  if(ceal_state->phase == CEAL_NO_CORE) {
    ceal_state->phase = CEAL_CORE_FROM_SCRATCH;
    ceal_cut_begin();    
  }
  else
    perror("ceal_core_begin");

#ifdef CEAL_ANALYTIC_STATS
  ceal_save_analytic_stats();
#endif
  
}

void ceal_core_end() {
  
  if(ceal_state->phase == CEAL_CORE_FROM_SCRATCH) {
    ceal_cut_end();
    ceal_state->phase = CEAL_CORE_COMPLETE;
  }
  else
    perror("ceal_core_end");

  /* Indicate the global end-time. */
  ceal_tvsig_invoke_begin( -1, __FILE__, __FUNCTION__, __LINE__, -1,
                           ceal_state->last,
                           ceal_state->last,
                           ceal_state->last->desc->size );
  ceal_tvsig_end();
  ceal_tvsig_invoke_end();
      
#ifdef CEAL_DEBUG
  /* Check that the stack is empty (it should be). */
  /* If this check fails, it means that there are some stack pushes
     that were never popped.  Use the magic numbers in ceal.log to
     find out which ones.  I currently believe such errors occur
     because of the way we encode/construct cuts from the CIL
     representation. This compiler issue is still outstanding.  */
  assert( ceal_stack_isempty( ceal_state->stack ) );
#endif
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_save_analytic_stats();

  /* Take a snapshot after the from-scratch run. */
  ceal_state->analytic_stats__from_scratch =
    ceal_state->analytic_stats ;  
#endif
  
  logg(PLUS_SEP_20 "Core " PLUS_SEP_20);
}

void ceal_core_reset() {
  /* TODO -- Free any memory associated with the core; reset the state
     of things as if the core program never ran. */
  ceal_state->phase = CEAL_NO_CORE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Cuts -- Functions for marking the begin / end of (dynamic) regions */

void ceal_cut_begin() {

  logg(MINUS_SEP_08 " C- scope %p", ceal_state->scope);

  /* Push the scope */
  ceal_stack_push( ceal_state->stack,
                   ceal_state->scope );

#ifdef CEAL_DEBUG
  /* Magic numbers help us in the log. */
  void* magic_num = (void*) ((uintptr_t) rand());
  ceal_stack_push( ceal_state->stack, magic_num );
  logg( "pushed magic number %p", magic_num );
#endif
  
  /* Push a NULL-marker. */
  ceal_stack_push( ceal_state->stack, NULL  );

}

void ceal_cut_end_with(ceal_trnode_t* cut_end_trnode0) {

  logg(FMT_TRNODE, VAS_TRNODE(cut_end_trnode0));

  if(!ceal_stack_peek(ceal_state->stack)) {
    /* Corner case: no trace nodes within the cut that require an
       end-time. So, avoid creating an end time.  We are done. */
    ceal_stack_pop(ceal_state->stack);
  }
  else {
  
    /* Desc for trace nodes that mark the end of cuts */
    static ceal_desc_t cut_end_desc =
      CEAL_DESC_DUMMY(sizeof(ceal_desc_t*) + sizeof(ceal_time_t));
    
    ceal_trnode_t* cut_end_trnode =
      ( cut_end_trnode0
        ? cut_end_trnode0
        : ceal_trnode_new(& cut_end_desc) );
    
    ceal_time_t* end_time = & cut_end_trnode->start_time;
    
    ceal_trnode_t* trnode;
    
    while(trnode = ceal_stack_pop( ceal_state->stack ),
          trnode != NULL) {
      
#ifdef CEAL_DEBUG
      assert( ceal_trnode_desc(trnode)->has_end_time );
#endif     
      
      trnode->end_time = end_time;
      
      logg(FMT_TRNODE " ends with " FMT_TRNODE,
           VAS_TRNODE(trnode), cut_end_trnode);
    }

    
    if( cut_end_trnode0 ) {
      /* Do something here? */
    }
    else {
      /* We created a new end-time. */
      ceal_tvsig_invoke_begin( -1, __FILE__, __FUNCTION__, __LINE__, -1,
                               cut_end_trnode,
                               cut_end_trnode,
                               cut_end_desc.size );        
      ceal_tvsig_end();
      ceal_tvsig_invoke_end();
    }
  }

#ifdef CEAL_DEBUG
  /* Magic numbers help us in the log. */
  void* magic_num = ceal_stack_pop( ceal_state->stack );
  logg( "popped magic number %p", magic_num );
#endif
  
  /* Restore the scope. */
  ceal_state->scope =
    ceal_stack_pop(ceal_state->stack);
  
  logg(PLUS_SEP_08 " C+ scope %p", ceal_state->scope);
}

void ceal_cut_end() {
  ceal_cut_end_with(NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Unboxed allocation -- Invoke, Revinv & Revoke */

void* ceal_unboxed_invoke(ceal_trnode_t* trnode,
                          void* pointer,
                          uintptr_t sz) {

  logg(FMT_TRNODE        " A- handle %p, size %ld",
       VAS_TRNODE(trnode),    pointer,   sz);

  memset(pointer, 0, sz);

  logg(FMT_TRNODE        " A+ handle %p, size %ld",
       VAS_TRNODE(trnode),    pointer,   sz);

  return pointer;
}

void* ceal_unboxed_revinv(ceal_trnode_t* trnode,
                          void* pointer,
                          uintptr_t sz) {

  logg(FMT_TRNODE        " A- handle %p, size %ld",
       VAS_TRNODE(trnode),    pointer,   sz);

  /* Do nothing. */
  /* Just return the same pointer. */

  /* Note: It's an invariant of the code generated by our compiler
     that unboxed allocations never follow reads within a trace
     node. */
  
  logg(FMT_TRNODE        " A+ handle %p, size %ld",
       VAS_TRNODE(trnode),    pointer,   sz);

  return pointer;
}


void* ceal_unboxed_revoke(ceal_trnode_t* trnode,
                          void* pointer,
                          uintptr_t sz) {

  logg(FMT_TRNODE        " A- handle %p, size %ld",
       VAS_TRNODE(trnode),    pointer,   sz);

  memset(pointer, 0, sz);

  logg(FMT_TRNODE        " A+ handle %p, size %ld",
       VAS_TRNODE(trnode),    pointer,   sz);

  return pointer;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Boxed allocation -- Invoke, Revinv & Revoke */

void*
ceal_alloc_invoke(ceal_trnode_t* trnode,
                  ceal_alloch_t* alloch,
                  uintptr_t sz) {
  
  logg(FMT_TRNODE        " A- handle %p, size %ld",
       VAS_TRNODE(trnode),    alloch,    sz);

  ceal_box_t* box = basemm_malloc(sizeof(ceal_box_t) + sz);

  box->size = sz;
  box->next = NULL;

  /* Zero out the memory. */
  memset(box->data, 0, sz);
  
  if( trnode == NULL ) {
    /* If no trace node, then its a meta-level allocation.  We add the
       allocation to the current metaboxes allocation handle, if its
       non-NULL. */
#ifdef CEAL_DEBUG
    assert ( alloch == NULL );
#endif
    if( ceal_state->metaboxes ) {
      box->next = ceal_state->metaboxes->box,
        ceal_state->metaboxes->box = box;
    }
  }
  else {
    alloch->box = box;
  }

  logg(FMT_TRNODE        " A+ handle %p, size %ld, box %p, pointer %p",
       VAS_TRNODE(trnode),    alloch,    sz,       box,    box->data);
  
  return box->data;
}

void
ceal_alloc_revoke(ceal_trnode_t* trnode,
                  ceal_alloch_t* alloch) {

  alloch->box->next = ceal_state->garbage.boxes;
  ceal_state->garbage.boxes = alloch->box;
}

void
ceal_alloc_kill(void* ptr) {
  ceal_alloch_t alloch = { & ((ceal_box_t*) ptr)[-1] };

  logg("K- pointer %p, box %p", ptr, alloch.box);

  if( ptr )
    ceal_alloc_revoke(NULL, &alloch);

  logg("K+ pointer %p, box %p", ptr, alloch.box);
}

void*
ceal_alloc_revinv(ceal_trnode_t* trnode,
                  ceal_alloch_t* alloch,
                  uintptr_t sz) {

#ifdef CEAL_DEBUG
  assert ( trnode != NULL && alloch != NULL );
#endif
  
  ceal_alloc_revoke(trnode, alloch);
  return ceal_alloc_invoke(trnode, alloch, sz);
}


ceal_alloch_t* ceal_metaboxes_new () {
  ceal_alloch_t* alloch0 = ceal_state->metaboxes;
  ceal_state->metaboxes = basemm_malloc( sizeof( ceal_alloch_t ) );
  ceal_state->metaboxes->box = NULL;
  return alloch0; 
}

ceal_alloch_t* ceal_metaboxes_get () {
  return ceal_state->metaboxes;
}

ceal_alloch_t* ceal_metaboxes_set ( ceal_alloch_t* metaboxes ) {
  ceal_alloch_t* alloch0 = ceal_state->metaboxes;
  ceal_state->metaboxes = metaboxes;
  return alloch0;
}

void ceal_metaboxes_kill ( ceal_alloch_t* metaboxes ) {
  ceal_box_t* box = metaboxes->box;

  while( box ) {
    ceal_box_t* next_box = box->next;
    /* Insert into garbage. */
    box->next = ceal_state->garbage.boxes,
      ceal_state->garbage.boxes = box;
    /* Onto the next box. */
    box = next_box;
  }
  
  basemm_free( sizeof(ceal_alloch_t), metaboxes );
}

void ceal_metaboxes_free ( ceal_alloch_t* metaboxes ) {
  ceal_box_t* box = metaboxes->box;

  while( box ) {
    ceal_box_t* next_box = box->next;
    /* Free immediately. */
    basemm_free ( sizeof(ceal_box_t) + box->size, box );
    /* Onto the next box. */
    box = next_box;
  }
  
  basemm_free( sizeof(ceal_alloch_t), metaboxes );

}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Propagation */

static void
ceal_trnode_free(ceal_trnode_t* trnode) {
  logg(FMT_TRNODE " free-", VAS_TRNODE(trnode));

  uintptr_t size = ceal_trnode_desc( trnode )->size;

#ifdef CEAL_ANALYTIC_STATS
  ceal_desc_stats_by_phase(ceal_trnode_desc(trnode))->free ++;
#endif
  
#ifdef CEAL_DEBUG
  /* NEW */
  memset( trnode, 0, size );
#endif

  basemm_free( size, trnode );

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.trnode_free ++;  
#endif

  ceal_tvsig_freed( trnode );
  
  logg(FMT_TRNODE " free+", VAS_TRNODE(trnode));
}

static void
ceal_trnode_collect(ceal_trnode_t* trnode) {
  logg(FMT_TRNODE " collect-", VAS_TRNODE(trnode));

#if CEAL_DEBUG
  /* Pre-condition: not on the queue. */
  assert( ! ceal_trnode_is_enqueued( trnode ) );
#endif

  /* When can we free this? */
  if( ! ceal_trnode_desc( trnode )->has_unboxed ) {

    /* No unboxed allocations; So, free now. */
    ceal_trnode_free( trnode );

  }
  else {
    logg(FMT_TRNODE " has unboxed allocs; adding to garbage.",
         VAS_TRNODE(trnode));

    ceal_trnode_garb_t* garb =
      (ceal_trnode_garb_t*) trnode;
    
    /* Has unboxed allocations; free later. */
    garb->next = ceal_state->garbage.trnodes;
    ceal_state->garbage.trnodes = garb;
  }

  ceal_tvsig_collected( trnode );
  
  logg(FMT_TRNODE " collect+", VAS_TRNODE(trnode));
}

static void
ceal_trnode_undo(ceal_trnode_t* trnode) {

  logg(FMT_TRNODE " undo-", VAS_TRNODE(trnode));

#ifdef CEAL_ANALYTIC_STATS
  ceal_desc_stats_by_phase(ceal_trnode_desc(trnode))->undo ++;
#endif

  /* Call undo function. */
  if( ceal_trnode_desc(trnode)->undo ) {
    ceal_trnode_desc(trnode)->undo ( trnode );
  }

  /* If not enqueued, collect the trace node as garbage. */
  if( ! ceal_trnode_is_enqueued( trnode ) ) {  
    ceal_trnode_collect( trnode );
  }
  else {
    logg(FMT_TRNODE " still enqueued; deferring collection to later.",
         VAS_TRNODE(trnode));

    /* Zero out the time stamp. (Since it's Still enqueued, this
       prevents following dangling pointers when performing timestampq
       comparisons on it) */
    memset(&trnode->start_time, 0, sizeof(ceal_time_t));
  }

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.trnode_undo ++;
#endif
  
  logg(FMT_TRNODE " undo+", VAS_TRNODE(trnode));
}

void ceal_time_undo(ceal_time_t* time){

  ceal_trnode_undo( ceal_trnode_of_time ( time ) );
  
}

/* Remove an interval (exclusive) */
static void
ceal_undo_interval(ceal_time_t* start,
                   ceal_time_t* end) {
  
  logg("undo- from " FMT_TRNODE " to " FMT_TRNODE,
       VAS_TRNODE(ceal_trnode_of_time(start)),
       VAS_TRNODE(ceal_trnode_of_time(end)));

  totalorder_remove_interval(start, end);

  logg("undo+ from " FMT_TRNODE " to " FMT_TRNODE,
       VAS_TRNODE(ceal_trnode_of_time(start)),
       VAS_TRNODE(ceal_trnode_of_time(end)));
}


/* Redo a trace node.
   Includes undo of any leftover (unused) trace within its interval. */
static void
ceal_trnode_redo(ceal_trnode_t* trnode) {

  logg(FMT_TRNODE " redo-", VAS_TRNODE(trnode));

  /* Restore the scope, or set it to NULL */
  if( ceal_trnode_desc(trnode)->saves_scope )
    ceal_state->scope = ceal_trnode_scope( trnode );
  else
    ceal_state->scope = NULL;        
  
  /* Redo the trace node. */ {    
    ceal_state->time_now  = ceal_trnode_time( trnode );

    ceal_tvsig_redo(trnode);
    
    /* Note: we save the time_redo_end global variable here because it
       may be changed by the redo() code that we run below. */
    /* In the tracing machine, this corresponds to saving the
       appropriate marker in the trace context. */
    ceal_time_t* time_redo_end = NULL;
    
    if( ceal_trnode_desc( trnode )->has_end_time ) {
#ifdef CEAL_DEBUG
      assert( trnode->end_time );
#endif
      ceal_state->time_redo_end = trnode->end_time;
      time_redo_end             = trnode->end_time;
    }
    else {
      ceal_state->time_redo_end = ceal_trnode_time( trnode );
      time_redo_end             = ceal_trnode_time( trnode );
    }
    
    ceal_cut_begin();
    
    logg(FMT_TRNODE " redo()- " FMT_DESC,
         VAS_TRNODE(trnode),
         VAS_DESC(ceal_trnode_desc(trnode)));
    
    ceal_trnode_desc(trnode)->redo( trnode );
    
    logg(FMT_TRNODE " redo()+ " FMT_DESC,
         VAS_TRNODE(trnode),
         VAS_DESC(ceal_trnode_desc(trnode)));

    /* Note: we use time_redo_end, saved above */
    ceal_cut_end_with( ceal_trnode_of_time ( time_redo_end ) );
  }
  
  /* Remove the old, unused trace, if any. */
  if( ceal_trnode_desc(trnode)->has_end_time ) {
    
    ceal_undo_interval( ceal_state->time_now,
                        trnode->end_time );
    
    /* Update the time. */
    ceal_state->time_now = trnode->end_time;              
  }
  else {
#ifndef CEAL_DEBUG
    /* Sanity check: if no end time, then no new stuff. */
    assert( ceal_state->time_now == ceal_trnode_time( trnode ) );
#endif
  }

  logg(FMT_TRNODE " redo+", VAS_TRNODE(trnode));
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.trnode_redo ++;
  ceal_desc_stats_by_phase(ceal_trnode_desc(trnode))->redo ++;
#endif
}


/* Propagation within an interval (inclusive) */
static void
ceal_propagate_interval(ceal_trnode_t* start,
                        ceal_trnode_t* end) {

  logg(MINUS_SEP_20 "Propagate " MINUS_SEP_20);
  logg("from " FMT_TRNODE " to "  FMT_TRNODE,
       /*   */ VAS_TRNODE(start), VAS_TRNODE(end));
  
  ceal_state->time_now = ceal_trnode_time(start);

  while(! ceal_pqueue_isempty(ceal_state->pqueue) &&
        ceal_trnode_compare(ceal_pqueue_peek(ceal_state->pqueue), end) <= 0)
  {
    ceal_trnode_t* trnode = ceal_trnode_dequeue();

    /* Is this trace node old? If so, free it and continue. */
    if( totalorder_compare ( ceal_trnode_time( trnode ),
                             ceal_state->time_now ) < 0 ) {

      logg(FMT_TRNODE " is old; it will now be collected.",
           VAS_TRNODE( trnode ) );
      
      /* It's already been undone.  Collect it. */
      ceal_trnode_collect( trnode );
      continue;
    }
    else {
      logg(FMT_TRNODE " is not old; redoing it ...",
           VAS_TRNODE( trnode ) );
      
      /* Redo it. */
      ceal_trnode_redo( trnode );
    }
  }

  ceal_state->time_now = ceal_trnode_time(end);
  
  ceal_tvsig_propto(end);

  logg(PLUS_SEP_20 "Propagate " PLUS_SEP_20);
}

void ceal_free_garbage() {

  logg("- collections %ld", ceal_state->garbage.collections );
  
  { /* Free the trace nodes. */
    ceal_trnode_garb_t* garb = ceal_state->garbage.trnodes;
  
    while( garb ) {
      ceal_trnode_garb_t* next = garb->next;
      ceal_trnode_free((ceal_trnode_t*) garb);
      garb = next;
    }
  }

  { /* Free the boxes. */
    ceal_box_t* garb = ceal_state->garbage.boxes;

    while( garb ) {
      ceal_box_t* next = garb->next;

      logg("freeing garbage size %ld, box %p, pointer %p",
           garb->size, garb, garb->data);
      
      basemm_free( garb->size, garb );
      garb = next;
    }
    
  }
  
  ceal_state->garbage.collections ++;
  ceal_state->garbage.trnodes = NULL;
  ceal_state->garbage.boxes   = NULL;

  logg("+");
}

void ceal_propagate() {
  logg(MINUS_SEP_20);

#ifdef CEAL_DEBUG
  assert( ceal_state->init_flags & CEAL_INIT_SELFADJ );
  
  assert( ceal_state->phase == CEAL_CORE_COMPLETE ||
          ceal_state->phase == CEAL_PROPAGATION_COMPLETE );
#endif

  ceal_dirtyset_clear();
  
  ceal_state->phase = CEAL_PROPAGATION_RUNNING;

  ceal_tvsig_meta_begin(-1, __FILE__, __FUNCTION__, __LINE__, -1);
  ceal_tvsig_m_prop_begin();
  ceal_tvsig_meta_end();
  
  ceal_propagate_interval(ceal_state->first,
                          ceal_state->last);
#ifdef CEAL_DEBUG
  assert( ceal_state->phase == CEAL_PROPAGATION_RUNNING );
#endif
  
  ceal_tvsig_meta_begin(-1, __FILE__, __FUNCTION__, __LINE__, -1);
  ceal_tvsig_m_prop_end();
  ceal_tvsig_meta_end();
  
  ceal_state->phase = CEAL_PROPAGATION_COMPLETE;

  ceal_dirtyset_assert_clear();
  
  ceal_free_garbage();

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.propagations ++;
  ceal_save_analytic_stats();  
#endif
 
  logg(PLUS_SEP_20);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Scope -- Invoke, Revinv & Revoke */

void* ceal_scope_invoke(ceal_trnode_t* trnode,
                        ceal_scopeh_t* scopeh) {

  logg(FMT_TRNODE      "S+ handle %p",
       VAS_TRNODE(trnode), scopeh);

  hashtbl_init(& scopeh->hashtbl, CEAL_HASHTBL_INIT_SIZE);
  
  logg(FMT_TRNODE      "S- handle %p",
       VAS_TRNODE(trnode), scopeh);

  return & scopeh->hashtbl;
}

void* ceal_scope_revinv(ceal_trnode_t* trnode,
                        ceal_scopeh_t* scopeh) {
  /* Do nothing. ? */
  return & scopeh->hashtbl;
}

void  ceal_scope_revoke(ceal_trnode_t* trnode,
                        ceal_scopeh_t* scopeh) {
  
  logg(FMT_TRNODE      "+ handle %p",
       VAS_TRNODE(trnode), scopeh);

  hashtbl_zero(& scopeh->hashtbl );
  
  logg(FMT_TRNODE      "- handle %p",
       VAS_TRNODE(trnode), scopeh);  
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Update points -- */

void
ceal_update_invoke(ceal_trnode_t*   trnode,
                   ceal_updateh_t*  updateh /* handle */ ) {
  /* Nothing to do. */
}
                 
void
ceal_update_revoke(ceal_trnode_t*   trnode,
                   ceal_updateh_t*  memoh) {
  /* Nothing to do. */  
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Memoization -- Invoke & Revoke */

static uintptr_t
ceal_memo_keys_are_equal(ceal_trnode_t* trnode1,
                         ceal_trnode_t* trnode2,
                         uintptr_t byteoff,
                         uintptr_t bytec) {
  return 
    0 == memcmp( ((unsigned char*) trnode1) + byteoff,
                 ((unsigned char*) trnode2) + byteoff,
                 bytec ) ;
}

ceal_trnode_t*
ceal_memo_invoke(ceal_trnode_t* trnode,
                 ceal_memoh_t*  memoh, /* handle */
                 void*          memot, /* memo table */
                 void*          bytes, /* memo keys */
                 uintptr_t      bytec  /* key length */ ) {

#ifdef CEAL_DEBUG
  assert ( ceal_trnode_desc( trnode )->has_memo_entry );
#endif
  
#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.memo_invoke ++;
#endif

  /* Harmless cast. We use void* to keep it abstract outside of RT. */
  ceal_scopeh_t* scopeh = (ceal_scopeh_t*) memot;
  ceal_trnode_t*  match = NULL;
  uintptr_t        hash = 0xdeadbeef;
  
  logg(FMT_TRNODE     " M- handle %p, scope %p, keys %p, keyc %ld",
       VAS_TRNODE(trnode), memoh,     memot,    bytes,   bytec);

  /* Which scope shall we use? */
  if( scopeh ) {
    logg("scope is now %p", & scopeh->hashtbl);

    /* Set the scope to be the one given. */
    ceal_state->scope = & scopeh->hashtbl;
    
  }
  else {
    logg("using current scope %p", ceal_state->scope);

    /* Keep using the current one. */
    scopeh = ceal_state->scope;    
  }

#if CEAL_DEBUG
  assert( scopeh && "Scope is NULL.  Did you forget to create one?");
#endif
  
  /* Do we have a scope? */
  if( scopeh != NULL ) {

    /* Hash the keys. */
    hash = hash_buffer ( bytes, bytec,
                         (uintptr_t) ceal_trnode_desc( trnode ) );
    
    /* Are we finished with the from-scratch run? */
    if( CEAL_CORE_FROM_SCRATCH < ceal_state->phase ) {     

      /* The (byte) offset from the trnode to the keys. */
      uintptr_t byteoff = (uintptr_t)
        ((unsigned char*) bytes - (unsigned char*) trnode);
    
      /* Find the bucket list. */
      bucket_t* bucket = hashtbl_lookup ( & scopeh->hashtbl, hash );    
      
      while ( bucket ) {

#if CEAL_ANALYTIC_STATS
        ceal_state->analytic_stats.memo_search ++;
#endif              
        if(
           /* Quick check: Same hash value */
           bucket->hash == hash
           &&
           
           /* Soundness check: Same descriptor ==> Same memo point (statically). */
           ceal_trnode_desc( trnode ) == ceal_trnode_desc( bucket->trnode )
           &&

           /* Soundness check: The memoization keys are equal. */
           ceal_memo_keys_are_equal ( trnode, bucket->trnode, byteoff, bytec )
           &&
           
           /* Soundness check: Time is after current time. */
           totalorder_compare ( ceal_state->time_now,
                                ceal_trnode_time( bucket->trnode )) < 0
           &&
           
           /* Soundness check: Time is within the redo interval. */
           totalorder_compare ( ceal_trnode_time( bucket->trnode ),
                                ceal_state->time_redo_end ) < 0
           )
        { /* ==> Match! */
          /* Should we try to find the earliest match? */
          if(CEAL_MEMO_PREFERS_EARLIEST_MATCH) {
            /* Yes, we prefer the match that occurs earliest (in time order) */
            if( match &&
                totalorder_compare( ceal_trnode_time( match ) ,
                                    ceal_trnode_time( bucket->trnode ) ) < 0) {
              /* We already have a match, and it occurs earlier than this one. */
              bucket = bucket->next;
              continue;
            }
            else {
              /* Either this is the first match, or its the earlier one. */
              match = bucket->trnode;
              bucket = bucket->next;
              continue;
            }
          }
          else {
            /* First match we find is good enough. */
            match = bucket->trnode;
            break;
          }
        }
        
        else {
          /* No Match.  Next bucket. */
          bucket = bucket->next;
          continue;
        }
      }
    }
  }
  
  /* Did we find a match? */
  if( match ) {
    /* Yes. */

#if CEAL_ANALYTIC_STATS
    ceal_state->analytic_stats.memo_hit ++;
#endif
    
    logg(FMT_TRNODE " successfully matched " FMT_TRNODE " " FMT_DESC,
         VAS_TRNODE( trnode ),
         VAS_TRNODE( match ),
         VAS_DESC( ceal_trnode_desc ( match )));

    /* Free the candidate; we don't need it anymore. */
    ceal_trnode_free( trnode );
    
    /* Undo interval from now up until match. */
    ceal_undo_interval( ceal_state->time_now,
                        ceal_trnode_time( match ) );

    ceal_tvsig_reuse( match );
    
    /* Does the match have an end-time? */
    if( ceal_trnode_desc( match )->has_end_time ) {
    
      /* Propagate the matched interval. */
      ceal_propagate_interval( match,
                               ceal_trnode_of_time ( match->end_time ) );
    }
    else {

      /* No end time ==> it is instantaneous.
         Propagate the matched trace node. */
      ceal_propagate_interval( match, match );
      
    }
  }
  else {
    /* No Match. */

#if CEAL_ANALYTIC_STATS
    ceal_state->analytic_stats.memo_miss ++;
#endif
    
    logg(FMT_TRNODE " failed to match",
         VAS_TRNODE ( trnode ));

    /* Insert the candidate into the trace. */
    ceal_trnode_insert( trnode );

    if ( scopeh ) {      
      /* Insert it into the memo table. */
      memoh->bucket.hash   = hash;
      memoh->bucket.trnode = trnode;      
      hashtbl_insert ( & scopeh->hashtbl, & memoh->bucket );
      
      logg(FMT_TRNODE " inserted into scope %p.",
           VAS_TRNODE (trnode),     & scopeh->hashtbl);
    }
  }

  logg(FMT_TRNODE " M+ handle %p, scope %p, bytes %p, bytec %ld,"
       " matched " FMT_TRNODE,
       VAS_TRNODE(trnode), memoh, memot, bytes, bytec,
       VAS_TRNODE(match));

  return match;
}
                 
void
ceal_memo_revoke(ceal_trnode_t* trnode,
                 ceal_memoh_t*  memoh) {

#ifdef CEAL_DEBUG
  assert ( ceal_trnode_desc( trnode )->saves_scope );
#endif

#ifdef CEAL_ANALYTIC_STATS
  ceal_state->analytic_stats.memo_revoke ++;
#endif
  
  hashtbl_t* hashtbl = (hashtbl_t*) trnode->scope;

#ifdef CEAL_DEBUG  
  assert ( hashtbl && "Cannot revoke from a NULL hashtbl");
#endif
  
  hashtbl_remove( hashtbl, & memoh->bucket );
}
