/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: trace.h
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

/* trace.h -- This file provides type definitions and function
   prototypes related to generating and maintaining the execution
   trace of a CEAL core program. */

/* See also: runtime.h */

#ifndef __CEAL_TRACE_H__
#define __CEAL_TRACE_H__

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Trace node descriptors --- "descriptors" for short.
   
   Each descriptor corresponds to a chunk of CEAL core code that is
   traced as one inseparable unit.  Each time the chunk is traced, the
   RT generates a new trace node _instance_ that carries a pointer to
   the corresponding descriptor.

   Descriptors carry information that is (1) known statically and (2)
   shared across trace node instances, hence, in the target C code we
   represent these descriptors as static objects.
   
   Each descriptor contains:
   
   SIZE -- a size (in bytes) for its instances.  This includes
           everything, including the space for the pointer that points
           back to the descriptor.

   UNDO / REDO -- pointers to the undo / redo functions.

   FLAGS -- a set of flags that determine two things:
  
     (1) which fields are present in the header of each instance
   
     (2) when the start time is assigned to each instance (either
         immediately, or after a memo miss, in the case it
         has_memo_entry)

     Here are the flags:

     has_start_time -- for now, always true; In the future we should
        also support making the start time optional, but for the time
        being this would be too complicated to implement (and it would
        be useful only for trace nodes that exclusively contain either
        allocations and/or writes for single-write modrefs-- perhaps
        these are uncommon cases?)

     has_memo_entry -- when true, delay start time initialization
        until a memo miss occurs; upon a memo hit we used the matched
        node.
   
     saves_scope -- when true, we save the scope for one of two
        purposes (which are not mutually exclusive): (1) to manage the
        trace node within this scope (in the case that it is itself
        memoized), and (2) to restore the scope when and if the redo
        code executes (in this case, the scope is analygous to a
        hidden live variable that needs to be saved / restored)

     has_end_time -- when true, we assign the trace node an end time.
        Otherwise, the end-time of the trace node is not important.
        We require an end time depending on the structure of the trace
        node (does it contain a memo or any reads?) as well as the
        structure of the chunk of code it corresponds to (does the
        code end with a return, or a jump?).
*/

#include <stdint.h>

typedef enum ceal_init_flags_e {
  CEAL_INIT_NONE     = 0,
  CEAL_INIT_SELFADJ  = 1,
  CEAL_INIT_VERIFIER = 2,
  CEAL_INIT_TVSIG    = 4,
} ceal_init_flags_t ;

/* Initialize global state. */
void ceal_init( ceal_init_flags_t flags );

/* Get the intialization flags used. */
ceal_init_flags_t ceal_init_flags ();

/* Propagate all pending changes */
void ceal_propagate();

typedef struct ceal_desc_s       ceal_desc_t;
typedef struct ceal_desc_stats_s ceal_desc_stats_t;
typedef struct ceal_trnode_s     ceal_trnode_t;

typedef void (*ceal_redofn_t)(ceal_trnode_t* trnode);
typedef void (*ceal_undofn_t)(ceal_trnode_t* trnode);

struct ceal_desc_stats_s {
  uintptr_t new;
  uintptr_t redo;
  uintptr_t undo;
  uintptr_t free;
};

struct ceal_desc_s {
  uintptr_t     size;
  uintptr_t     has_start_time;
  uintptr_t     has_memo_entry;
  uintptr_t     has_unboxed;
  uintptr_t     saves_scope;
  uintptr_t     has_end_time;  
  ceal_redofn_t redo;
  ceal_undofn_t undo;
  /* - - - - - - - - - - - - - - - - - */
  const char*   name;
  const char*   file;
  long          line;
  const char*   debuginfo;
  ceal_desc_t*  next;
  ceal_desc_stats_t* stats_fromscratch;
  ceal_desc_stats_t* stats_propagation;
};

#define CEAL_DESC_DUMMY(size) {                 \
    size, 1, 0, 0, 0, 0, NULL, NULL,            \
      NULL, NULL, -1,                           \
      "CEAL_DESC_DUMMY",                        \
      NULL, NULL, NULL }

/* This is a pointer of some more specific type (see below) */
typedef void* ceal_scope_t;

#if 0
#define FMT_DESC \
    "desc %p size %d, "\
  "has_start_time %d, "\
     "has_unboxed %d, "\
  "has_memo_entry %d, "\
     "saves_scope %d, "\
    "has_end_time %d"
#else
#define FMT_DESC \
    "desc %p sz %d, "\
    "hst %d, "\
    "hme %d, "\
    "hub %d, "\
     "ss %d, "\
    "het %d, "\
    "%s"
#endif

#define VAS_DESC(desc) \
  desc, (int) desc->size,           \
        (int) desc->has_start_time, \
        (int) desc->has_memo_entry, \
        (int) desc->has_unboxed,    \
        (int) desc->saves_scope,    \
        (int) desc->has_end_time,   \
              desc->debuginfo

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Time stamps -- total order maintenance data structure. */

#include "totalorder.h"

typedef to_node_t ceal_time_t;

void  ceal_time_undo(ceal_time_t* time);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Trace nodes -- a central data-structure in our runtime system.
   Collectively, they represent the execution trace of a core CEAL
   program.

   Each instance consists of some prefix of the ceal_trnode_s
   structure below.  Every instance contains the first field (the
   descriptor).  The descriptor (see above) contains flags that
   indicate which other fields are present (which again, always form a
   prefix of those listed below)
   
   Roughly speaking, each trace node corresponds to the trace of a
   single basic block of CEAL code, i.e., an unbroken sequence of code
   whose control flow is always "straight-line".  This code may
   contain one or more traced operations (e.g., memos, reads, writes,
   allocations).
     
   Handles -- For each traced operation, we store a cooresponding
   'handle' in the trace node.  This handle consists of whatever
   satellite information is needed to support the invoke/revinv/revoke
   interface for the given operation.  From the compiler's
   prespective, this satellite data is abstract.  The handles are
   stored immediately after (some prefix of) the structure below.
*/

struct ceal_trnode_s {
  ceal_desc_t* desc;
  ceal_time_t  start_time;
  ceal_scope_t scope;    
  ceal_time_t* end_time;
};

ceal_trnode_t* ceal_trnode_new(ceal_desc_t* desc);
ceal_time_t*   ceal_trnode_time(ceal_trnode_t* trnode);
ceal_scope_t   ceal_trnode_scope(ceal_trnode_t* trnode);
int            ceal_trnode_compare(ceal_trnode_t* trnode1, ceal_trnode_t* trnode2);
void           ceal_trnode_enqueue(ceal_trnode_t* trnode);
void           ceal_trnode_enqueue_if_in_future(ceal_trnode_t* trnode);
void           ceal_dirtyset_add(void* thing, void(*cb)(void* thing));

#define FMT_TRNODE        "trnode %p"
#define VAS_TRNODE(trnode) trnode

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Core -- Functions for marking the begin / end of core trace     */

void ceal_core_begin();
void ceal_core_end();
void ceal_core_reset();

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Cuts -- Functions for marking the begin / end of (dynamic) regions */

void ceal_cut_begin();
void ceal_cut_end();

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Modifiable Types --- What kind of animal is it?  For now we aren't
   concerned with the scalar type held by the modifiable.  Instead, we
   care about the modifiable implementation itself.  We want to
   provide debugging messages for when a modifiable of one type is
   used at a different type. */

#ifdef CEAL_DEBUG_MODTYP
/* Though it would be nice to tag every modifiable in a uniform way
   (i.e., the tag is always placed in the same spot, no matter which
   tag it is..); this turned out to constrain the design of at least
   one modref variant; so I gave up on that.  As a compromise, the
   tags are entire words and don't look like pointers.  This means we
   can expand to an arbitrary number of variants (e.g., we aren't
   limited to those whose tags can fit into a few spare bits) */

/* These are just magic numbers that don't look like pointers (e.g.,
   their lowest bits are all set). */
typedef enum ceal_modtyp_e {
  CEAL_MODTYP_JUNK=0x0, /* Every non-junk tag is non-zero! */
  CEAL_MODTYP_ZWZR=0xFF00BEEF,
  CEAL_MODTYP_AWAR=0xAA00BEEF,
  CEAL_MODTYP_OWCR=0x1C00BEEF,
  CEAL_MODTYP_RING=0xD00BBEEE,
} ceal_modtyp_t;
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Scope -- Invoke, Revinv & Revoke */

#include "hashtbl.h"

typedef struct ceal_scopeh_s {
  hashtbl_t hashtbl;
} ceal_scopeh_t;

void* ceal_scope_invoke(ceal_trnode_t* trnode,
                        ceal_scopeh_t* scopeh);

void* ceal_scope_revinv(ceal_trnode_t* trnode,
                        ceal_scopeh_t* scopeh);

void  ceal_scope_revoke(ceal_trnode_t* trnode,
                        ceal_scopeh_t* scopeh);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Memoization -- Invoke & Revoke */

typedef struct ceal_updateh_s {
  void* __nothing[0];
} ceal_updateh_t;

void
ceal_update_invoke(ceal_trnode_t*   trnode,
                   ceal_updateh_t*  updateh /* handle */ );
                 
void
ceal_update_revoke(ceal_trnode_t*   trnode,
                   ceal_updateh_t*  memoh);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Memoization -- Invoke & Revoke */

typedef struct ceal_memoh_s {
  bucket_t bucket;
} ceal_memoh_t;

ceal_trnode_t*
ceal_memo_invoke(ceal_trnode_t* trnode,
                 ceal_memoh_t*  memoh, /* handle */
                 void*          memot, /* memo table */
                 void*          bytes, /* memo keys */
                 uintptr_t      bytec  /* key length */ );
                 
void
ceal_memo_revoke(ceal_trnode_t* trnode,
                 ceal_memoh_t*  memoh);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Unboxed Allocation -- Invoke, Revinv & Revoke */

typedef struct ceal_trnode_garb_s ceal_trnode_garb_t;

struct ceal_trnode_garb_s {
  ceal_desc_t*        desc;
  ceal_trnode_garb_t* next;  
};

void* ceal_unboxed_invoke(ceal_trnode_t* trnode,
                          void* unboxed_space,
                          uintptr_t sz);

void* ceal_unboxed_revinv(ceal_trnode_t* trnode,
                          void* unboxed_space,
                          uintptr_t sz);

void* ceal_unboxed_revoke(ceal_trnode_t* trnode,
                          void* unboxed_space,
                          uintptr_t sz);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* (Boxed) Allocation -- Invoke, Revinv & Revoke */
typedef struct ceal_box_s ceal_box_t;

struct ceal_box_s {
  uintptr_t   size;
  ceal_box_t* next;
  void*       data[0];
};

typedef struct ceal_alloch_s {
  ceal_box_t* box;
} ceal_alloch_t;

void* ceal_alloc_invoke(ceal_trnode_t* trnode,
                        ceal_alloch_t* alloch,
                        uintptr_t sz);

void* ceal_alloc_revinv(ceal_trnode_t* trnode,
                        ceal_alloch_t* alloch,
                        uintptr_t sz);

void  ceal_alloc_revoke(ceal_trnode_t* trnode,
                        ceal_alloch_t* alloch);       

void  ceal_alloc_kill(void* ptr);

/* Metaboxes Interface.
   
   Provides coarse-grained memory management at the meta level
   (coarse-grained means not using individual calls to
   ceal_alloc_kill):

   "Metaboxes" is the name of a special allocation handle that records
   allocations performed at the meta-level.  More than one handle can
   be used in this way, but at most one is active at any point.

   -- metaboxes_get : returns the active handle

   -- metaboxes_set : sets the given handle as the active one, returns
      the previously active handle.

   -- metaboxes_new : like metaboxes_set, except the active handle is
      made fresh.

   -- metaboxes_kill : marks all allocations associated with the
      handle garbage.

   -- metaboxes_free : immediately frees all allocations associated
      with the handle (use this with care --- in the presence of a
      self-adjusting core program, this can lead to dangling pointers
      if the memory is still referenced by the core's trace.
*/

ceal_alloch_t* ceal_metaboxes_new  ();
ceal_alloch_t* ceal_metaboxes_get  ();
ceal_alloch_t* ceal_metaboxes_set  ( ceal_alloch_t* metaboxes );
void           ceal_metaboxes_kill ( ceal_alloch_t* metaboxes );
void           ceal_metaboxes_free ( ceal_alloch_t* metaboxes );

#endif
