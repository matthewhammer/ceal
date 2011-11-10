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

/* Matthew A Hammer <hammer@tti-c.org> */

#include "logging.h"
#include "basemm.h"
#include "pqueue.h"

#include <string.h>
#include <stdlib.h>

void ceal_pqueue__grow(ceal_pqueue_t* pq);
void ceal_pqueue__bubble_up(ceal_pqueue_t* pq, int pos);
void ceal_pqueue__bubble_down(ceal_pqueue_t* pq, int pos);

#define UP(pos) ((pos - 1) >> 1)
#define DOWN(pos) ((pos << 1) + 1)
#define SETPOS(pq, a, pos) ((pq)->data[pos] = a)

static int 
compare(ceal_pqueue_t* pq, ceal_pqueue_elt_t e1, ceal_pqueue_elt_t e2) {
  return ceal_trnode_compare(e1, e2);
}

static void
swap(ceal_pqueue_t* pq, uintptr_t pos1, uintptr_t pos2) {
  ceal_pqueue_elt_t e1 = pq->data[pos1];
  ceal_pqueue_elt_t e2 = pq->data[pos2];
  pq->data[pos1] = e2;
  pq->data[pos2] = e1;
}

ceal_pqueue_t* ceal_pqueue_init(ceal_pqueue_t* pq) {
  pq->size = 16;
  pq->num = 0;
  pq->data = (ceal_pqueue_elt_t*)
    basemm_malloc(pq->size * sizeof(ceal_pqueue_elt_t));
  return pq;
}

uintptr_t ceal_pqueue_isempty(const ceal_pqueue_t* pq) {
  return pq->num == 0;
}

void ceal_pqueue__bubble_up(ceal_pqueue_t* pq, int pos) {
  while(pos > 0) {
    int up = UP(pos);
    if (compare(pq, pq->data[pos], pq->data[up]) < 0) {
      swap(pq, pos, up);
      pos = up;
    }
    else
      return;
  }
}

void ceal_pqueue_push(ceal_pqueue_t* pq, ceal_pqueue_elt_t elt) {
  if(pq->num == pq->size)
    ceal_pqueue__grow(pq);
  SETPOS(pq, elt, pq->num ++);
  ceal_pqueue__bubble_up(pq, pq->num - 1);

  logg("pqueue newsize is %d", (int) pq->num);
}

ceal_pqueue_elt_t ceal_pqueue_peek(const ceal_pqueue_t* pq) {
  return pq->data[0];
}

void ceal_pqueue__bubble_down(ceal_pqueue_t* pq, int pos) {
  int down;

  while((down = DOWN(pos)) < pq->num) {
    int tmp = down + 1;
    
    if ((tmp < pq->num) &&
        compare(pq, pq->data[tmp], pq->data[down]) < 0) {
      down = tmp;
    }
    
    if (compare(pq, pq->data[pos], pq->data[down]) > 0) {
      swap(pq, pos, down);
      pos = down;
    }
    else
      break;
  }
}

ceal_pqueue_elt_t ceal_pqueue_pop(ceal_pqueue_t* pq) {
  ceal_pqueue_elt_t elt_out = pq->data[0];
  swap(pq, 0, -- pq->num);
  ceal_pqueue__bubble_down(pq, 0);
  return elt_out;
}

void ceal_pqueue_cleanup(ceal_pqueue_t* pq) {
  basemm_free(pq->size * sizeof(ceal_pqueue_elt_t), pq->data);
  pq->data = NULL;
}

void ceal_pqueue__grow(ceal_pqueue_t* pq) {
  ceal_pqueue_elt_t* tmp;
  uintptr_t newsize = pq->size * 2;
  tmp = basemm_malloc(newsize * sizeof(ceal_pqueue_elt_t));
  memcpy(tmp, pq->data, sizeof(ceal_pqueue_elt_t) * pq->num);
  ceal_pqueue_cleanup(pq);
  pq->size = newsize;
  pq->data = tmp;
  logg("pqueue newsize is %d", (int) newsize);
}
