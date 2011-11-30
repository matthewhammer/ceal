/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: pqueue.h
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

/* Matthew A Hammer <hammer@tti-c.org> */

#ifndef __CEAL_PQUEUE_H__
#define __CEAL_PQUEUE_H__

#include "trace.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Pqueue -- Priority queue; Orders the set of affected trace nodes
*/

typedef ceal_trnode_t* ceal_pqueue_elt_t;
typedef struct ceal_pqueue_s ceal_pqueue_t;

ceal_pqueue_t*    ceal_pqueue_init(ceal_pqueue_t* pq);
uintptr_t         ceal_pqueue_isempty(const ceal_pqueue_t* pq);
void              ceal_pqueue_push(ceal_pqueue_t* pq, ceal_pqueue_elt_t elt);
ceal_pqueue_elt_t ceal_pqueue_pop(ceal_pqueue_t* pq);
ceal_pqueue_elt_t ceal_pqueue_peek(const ceal_pqueue_t* pq);
void              ceal_pqueue_cleanup(ceal_pqueue_t* pq);

struct ceal_pqueue_s {
  uintptr_t size;
  uintptr_t num;
  ceal_pqueue_elt_t* data;
};

#endif
