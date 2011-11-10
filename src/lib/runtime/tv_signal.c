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

#include <stdio.h>
#include <stdarg.h>
#include "trace.h"
#include "tv_signal.h"

FILE* ceal_tvsig_file = NULL;

#define FPF(fmt, ...) \
  (ceal_tvsig_file ? fprintf(ceal_tvsig_file, fmt,##__VA_ARGS__) : 0)

#define VFPF(fmt, ap) \
  (ceal_tvsig_file ? vfprintf(ceal_tvsig_file, fmt, ap) : 0)

#define TRNODEF "\"%p\""
#define TRNODEA(trnode) (trnode)

#define AIDF "(\"%p\",\"%p\")"
#define AIDA(trnode,handle) ((void*)trnode),((void*)(handle))


void ceal_tvsig_init(FILE* file) {
  ceal_tvsig_file = file;

  if( file ) {
#ifdef CEAL_DEBUG
    /* Don't do any buffering of the log. */
    /* (To diagnose potential segfaults).  */
    setvbuf(ceal_tvsig_file, NULL, _IONBF, 0);
#else
    /* Do line buffering. */
    /* (For efficiency) */
    setvbuf(ceal_tvsig_file, NULL, _IOLBF, 0);
#endif
  }
}

void ceal_tvsig_stop() {
  ceal_tvsig_init( NULL );
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
int ceal_tvsig_comma_needed(int clear, int set) {
  static int needed_bit = 0;
  int out = needed_bit;
  if(clear) needed_bit = 0;
  if(set)   needed_bit = 1;
  return out;
}

void ceal_tvsig_vals_begin() { FPF("["); ceal_tvsig_comma_needed(1,0); }
void ceal_tvsig_vals_val(const char* typ, const char* valfmt, ...) { 
  va_list ap; va_start(ap, valfmt);
  if( ceal_tvsig_comma_needed(0,1) ) FPF(",");
  FPF("("); VFPF(valfmt, ap); FPF(",\"%s\")",typ);
  va_end(ap); 
}
void ceal_tvsig_vals_end() { FPF("]"); }
   
void ceal_tvsig_env_begin() { FPF("["); ceal_tvsig_comma_needed(1,0); }
void ceal_tvsig_env_var(const char* var, const char* typ, const char* valfmt, ...) {
  va_list ap; va_start(ap, valfmt);
  if( ceal_tvsig_comma_needed(0,1) ) FPF(",");
  FPF("(\"%s\",",var); VFPF(valfmt, ap); FPF(",\"%s\")",typ);
  va_end(ap);
}
void ceal_tvsig_env_end() { FPF("]"); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* 
   A_ = Action descriptors. 
*/

void ceal_tvsig_alloc(const char* typ, uintptr_t sz, void* ptr) { 
  FPF("<\"A_alloc\":(\"%s\",%ld,\"%p\")>",typ,(long)sz,ptr);
}

void ceal_tvsig_scope(void* ptr) { 
  FPF("<\"A_scope\":\"%p\">",ptr);
}

void ceal_tvsig_read(const char* typ, const char* qual, void* ptr, const char* valfmt, ...) {
  va_list ap; va_start(ap, valfmt);
  FPF("<\"A_read\":(\"%s\",\"%s\",\"%p\",",typ,qual,ptr);
  VFPF(valfmt,ap); FPF(")>");
  va_end(ap);
}

void ceal_tvsig_write(const char* typ, const char* qual, void* ptr, const char* valfmt, ...) {
  va_list ap; va_start(ap, valfmt);
  FPF("<\"A_write\":(\"%s\",\"%s\",\"%p\",",typ,qual,ptr);
  VFPF(valfmt,ap); FPF(")>");
  va_end(ap);
}

void ceal_tvsig_update_begin() { FPF("<\"A_update\":"); }
void ceal_tvsig_update_end() { FPF(">"); }

void ceal_tvsig_memo_begin() { FPF("<\"A_memo\":"); }
void ceal_tvsig_memo_end() { FPF(">"); }

void ceal_tvsig_tcall_begin(const char* fun) { FPF("<\"A_tcall\":(\"%s\",",fun); }
void ceal_tvsig_tcall_end() { FPF(")>"); }

void ceal_tvsig_push_begin() { FPF("<\"A_push_begin\">"); }
void ceal_tvsig_push_end() { FPF("<\"A_push_end\">"); }

void ceal_tvsig_pop_begin() { FPF("<\"A_pop\":"); }
void ceal_tvsig_pop_end() { FPF(">"); }

void ceal_tvsig_end() { FPF("<\"A_end\">"); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* 
   S_ = Steps of tracing machine. 

   Each step is sent as an event. 
   (it is wrapped with E_step).
 */

static long ceal_tvsig_null_counter = 1L;

/* S_invoke (begin) */
void ceal_tvsig_invoke_begin
( /* ppt components */
  int ppt_uid, const char* ppt_file, const char* ppt_fname, int ppt_line, int ppt_byte,
  ceal_trnode_t* trnode, void* handle, /* action_id components */
  uintptr_t sz                         /* size of action. */
  ) {

  /* We want unique IDs. So, for NULL trace nodes, we assign a unique
     counter number as the handle. */
  if (trnode == NULL) {
    handle = (void*) ((uintptr_t) ceal_tvsig_null_counter++);
  }
  
  FPF("<\"E_step\":<\"S_invoke\":("
      "{\"ppt_id\":%d,\"ppt_file\":\"%s\",\"ppt_fname\":\"%s\",\"ppt_line\":%d,\"ppt_byte\":%d},"AIDF",%ld,",
      ppt_uid, ppt_file, ppt_fname, ppt_line, ppt_byte, AIDA(trnode, handle), (long)sz);
}
/* action_desc goes here. */
/* S_invoke (end) */
void ceal_tvsig_invoke_end() { FPF(")>>\n"); }

/* S_revinv (begin) */
void ceal_tvsig_revinv_begin
( /* ppt components */
  int ppt_uid, const char* ppt_file, const char* ppt_fname, int ppt_line, int ppt_byte,
   ceal_trnode_t* trnode, void* handle, /* action_id components */
   uintptr_t sz                         /* size of action. */
  ) {
  FPF("<\"E_step\":<\"S_revinv\":("
      "{\"ppt_id\":%d,\"ppt_file\":\"%s\",\"ppt_fname\":\"%s\",\"ppt_line\":%d,\"ppt_byte\":%d},"AIDF",%ld,",
      ppt_uid, ppt_file, ppt_fname, ppt_line, ppt_byte, AIDA(trnode, handle), (long)sz);
}
/* action_desc goes here. */
/* S_revinv (end) */
void ceal_tvsig_revinv_end() { FPF(")>>\n"); }

/* S_revoke = an undo step */
void ceal_tvsig_revoke(ceal_trnode_t* trnode, void* handle) {
  FPF("<\"E_step\":<\"S_revoke\":"AIDF">>\n",AIDA(trnode,handle));
}

/* S_redo = prop --> eval @ update, i.e., P.E step. */
void ceal_tvsig_redo(ceal_trnode_t* trnode) {
  FPF("<\"E_step\":<\"S_redo\":"TRNODEF">>\n",TRNODEA(trnode));
}

/* S_reuse = eval --> prop @ memo, ie., E.P step. */
void ceal_tvsig_reuse(ceal_trnode_t* trnode) {
  FPF("<\"E_step\":<\"S_reuse\":"TRNODEF">>\n",TRNODEA(trnode));
}

void ceal_tvsig_propto(ceal_trnode_t* trnode) {
  FPF("<\"E_step\":<\"S_propto\":"TRNODEF">>\n",TRNODEA(trnode));
}

/* Meta-level steps. */
void ceal_tvsig_meta_begin
(  int ppt_uid, const char* ppt_file, const char* ppt_fname, int ppt_line, int ppt_byte /* ppt components */) {
  FPF("<\"E_meta\":("
      "{\"ppt_id\":%d,\"ppt_file\":\"%s\",\"ppt_fname\":\"%s\",\"ppt_line\":%d,\"ppt_byte\":%d},",
      ppt_uid, ppt_file, ppt_fname, ppt_line, ppt_byte);
}
/* action_desc goes here. */
/* S_invoke (end) */
void ceal_tvsig_meta_end() { FPF(")>\n"); }

void ceal_tvsig_m_core_begin() {
  FPF("<\"M_core_begin\">");
}

void ceal_tvsig_m_core_end() {
  FPF("<\"M_core_end\">");
}

void ceal_tvsig_m_prop_begin() {
  FPF("<\"M_prop_begin\">");
}

void ceal_tvsig_m_prop_end(){
  FPF("<\"M_prop_end\">");
}

void ceal_tvsig_m_alloc(const char* typ, uintptr_t sz, void* ptr) {
  FPF("<\"M_alloc\":(\"%s\",%ld,\"%p\")>",typ,(long)sz,ptr);
}

void ceal_tvsig_m_read(const char* typ, const char* qual, void* ptr, const char* valfmt, ...) {
  va_list ap; va_start(ap, valfmt);
  FPF("<\"M_read\":(\"%s\",\"%s\",\"%p\",",typ,qual,ptr);
  VFPF(valfmt,ap); FPF(")>");
  va_end(ap);
}

void ceal_tvsig_m_write(const char* typ, const char* qual, void* ptr, const char* valfmt, ...) {
  va_list ap; va_start(ap, valfmt);
  FPF("<\"M_write\":(\"%s\",\"%s\",\"%p\",",typ,qual,ptr);
  VFPF(valfmt,ap); FPF(")>");
  va_end(ap);
}

void ceal_tvsig_m_kill(void* ptr) {
  FPF("<\"M_kill\":\"%p\">", ptr);
}
/* 
   D_ = Declarations about state.

   Each declaration is sent as an event. 
   (it is wrapped with E_decl).
 */

void ceal_tvsig_allocated(ceal_trnode_t* trnode, const char* typ, uintptr_t sz) {
  FPF("<\"E_decl\":<\"D_allocated\":("TRNODEF",\"%s\",%ld)>>\n", trnode, typ, sz);
}

void ceal_tvsig_consistent(ceal_trnode_t* trnode) {
  FPF("<\"E_decl\":<\"D_consistent\":"TRNODEF">>\n",trnode); 
}

void ceal_tvsig_enqueued(ceal_trnode_t* trnode) {
  FPF("<\"E_decl\":<\"D_enqueued\":"TRNODEF">>\n",trnode); 
}

void ceal_tvsig_dequeued(ceal_trnode_t* trnode) {
  FPF("<\"E_decl\":<\"D_dequeued\":"TRNODEF">>\n",trnode); 
}

void ceal_tvsig_collected(ceal_trnode_t* trnode) {
  FPF("<\"E_decl\":<\"D_collected\":"TRNODEF">>\n",trnode); 
}

void ceal_tvsig_freed(ceal_trnode_t* trnode)  {
  FPF("<\"E_decl\":<\"D_freed\":"TRNODEF">>\n",trnode); 
}
