/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: tv_signal.h
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

/* Generate trace-visualization signals. */

#ifndef __CEAL_TVSIG_H__
#define __CEAL_TVSIG_H__

#include <stdio.h>

void ceal_tvsig_init(FILE* file);
void ceal_tvsig_stop();

/* Writing value sequences.  One value at a time. */
void ceal_tvsig_vals_begin();
void ceal_tvsig_vals_val(const char* typ, const char* valfmt, ...);
void ceal_tvsig_vals_end();
   
/* Writing environments.  One variable at a time. */
void ceal_tvsig_env_begin();
void ceal_tvsig_env_var(const char* var, const char* typ, const char* valfmt, ...);
void ceal_tvsig_env_end();

/* 
   A_ = Action descriptors. 
*/

/* Scopes, allocs, reads and writes. */
void ceal_tvsig_alloc(const char* typ, uintptr_t sz, void* ptr);
void ceal_tvsig_scope(void* ptr);
void ceal_tvsig_read(const char* typ, const char* qual, void* ptr, const char* valfmt, ...);
void ceal_tvsig_write(const char* typ, const char* qual, void* ptr, const char* valfmt, ...);

/* Update points. */
void ceal_tvsig_update_begin();
/* .. write environment .. */
void ceal_tvsig_update_end();

/* Memo points. */
void ceal_tvsig_memo_begin();
/* .. write environment .. */
void ceal_tvsig_memo_end();

/* Tail-recursive calls. */
void ceal_tvsig_tcall_begin(const char* fun);
/* .. write argument values .. */
void ceal_tvsig_tcall_end();

/* Pushes.  Note: In this context of tv signals, "pus_begin" and
   "push_end" are each actions unto themselves. */
void ceal_tvsig_push_begin();
void ceal_tvsig_push_end();

/* Pops. */
void ceal_tvsig_pop_begin();
/* .. write return values .. */
void ceal_tvsig_pop_end();

/* Ends. (end-times, here represented as an "action", shared amongst
   many nested intervals).  This isn't needed in the tracing machine,
   but is helpful in the RT implementation. */
void ceal_tvsig_end();

/* 
   S_ = Steps of tracing machine. 

   Each step is sent as an event. 
   (it is wrapped with E_step).
 */

/* S_invoke (begin) */
void ceal_tvsig_invoke_begin
( /* ppt components */
  int ppt_uid, const char* ppt_file, const char* ppt_fname, int ppt_line, int ppt_byte,
  ceal_trnode_t* trnode, void* handle, /* action_id components */
  uintptr_t sz                         /* size of action. */
   );
/* action_desc goes here. */
/* S_invoke (end) */
void ceal_tvsig_invoke_end();

/* S_revinv (begin) */
void ceal_tvsig_revinv_begin
( /* ppt components */
  int ppt_uid, const char* ppt_file, const char* ppt_fname, int ppt_line, int ppt_byte,
  ceal_trnode_t* trnode, void* handle, /* action_id components */
  uintptr_t sz                         /* size of action. */
  );

/* action_desc goes here. */
/* S_revinv (end) */
void ceal_tvsig_revinv_end();

/* S_revoke = an undo step */
void ceal_tvsig_revoke(ceal_trnode_t* trnode, void* handle);

/* S_redo = prop --> eval @ update, i.e., P.E step. */
void ceal_tvsig_redo(ceal_trnode_t* trnode);

/* S_reuse = eval --> prop @ memo, ie., E.P step. */
void ceal_tvsig_reuse(ceal_trnode_t* trnode);

/* S_propto = prop --> prop, over consistent actions. */
void ceal_tvsig_propto(ceal_trnode_t* trnode);

/* meta-level steps. */
void ceal_tvsig_meta_begin ( int ptt_uid, const char* ppt_file, const char* ppt_fname, int ppt_line, int ppt_byte );
void ceal_tvsig_meta_end();

/*
  M_ = Meta-level step descriptors.
*/
void ceal_tvsig_m_core_begin();
void ceal_tvsig_m_core_end();
void ceal_tvsig_m_prop_begin();
void ceal_tvsig_m_prop_end();
void ceal_tvsig_m_alloc(const char* typ, uintptr_t sz, void* ptr);
void ceal_tvsig_m_read(const char* typ, const char* qual, void* ptr, const char* valfmt, ...);
void ceal_tvsig_m_write(const char* typ, const char* qual, void* ptr, const char* valfmt, ...);
void ceal_tvsig_m_kill(void* ptr);

/* 
   D_ = Declarations about state.

   Each declaration is sent as an event. 
   (it is wrapped with E_decl).
 */

void ceal_tvsig_allocated(ceal_trnode_t* trnode, const char* typ, uintptr_t sz);
void ceal_tvsig_consistent(ceal_trnode_t* trnode);
void ceal_tvsig_enqueued(ceal_trnode_t* trnode);
void ceal_tvsig_dequeued(ceal_trnode_t* trnode);
void ceal_tvsig_collected(ceal_trnode_t* trnode);
void ceal_tvsig_freed(ceal_trnode_t* trnode);


#endif
