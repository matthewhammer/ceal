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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include "hashtbl.h"
#include "basemm.h"
#include "state.h"

static 
void hashtbl_move(hashtbl_t* src, hashtbl_t* dst) {
  uintptr_t  bucket_index;
  uintptr_t  src_size  = src->size;
  uintptr_t  mask      = dst->size - 1;
  bucket_t** dst_table = dst->table;
  
  for(bucket_index = 0; bucket_index < src_size; bucket_index++) {
    bucket_t* bucket = src->table[bucket_index];

    while(bucket) {
      bucket_t** head = &(dst_table[bucket->hash & mask]);
      bucket_t* old_next = bucket->next;
      bucket_t* new_next = *head;

#if CEAL_ANALYTIC_STATS
      ceal_state->analytic_stats.memo_move ++;
#endif
      
      /* Splice in the bucket: */
      bucket->next = new_next;
      *head = bucket;

      /* Go to the next bucket to process: */
      bucket = old_next;
    }
  }
  dst->num = src->num;
  src->num = 0;
}

static 
void hashtbl_resize(hashtbl_t* h, int newsize) {
  uintptr_t oldsize = h->size;
  hashtbl_t hh;
  hashtbl_init(&hh, newsize);
  if(newsize)
    hashtbl_move(h, &hh);
  basemm_free(oldsize * sizeof(bucket_t*), h->table);
  *h = hh;
}

hashtbl_t* hashtbl_init(hashtbl_t* h, int size) {
  
  h->size  = size;
  h->num   = 0;

  h->table = ( size
               ? basemm_malloc(size * sizeof(bucket_t*))
               : NULL );
  if(size)
    memset(h->table, 0, size * sizeof(bucket_t*));
  
  return h;
}

void hashtbl_zero(hashtbl_t* h) {
  hashtbl_resize(h, 0);
}

void
hashtbl_insert (hashtbl_t* h, bucket_t* bucket) {
  if(h->num >= h->size << CEAL_HASHTBL_MAXCAP_BITS) {
    hashtbl_resize(h, ((h->size > 0)
                       ? h->size << CEAL_HASHTBL_GROWTH_BITS
                       : CEAL_HASHTBL_INIT_SIZE));
  }
  bucket_t** head = &(h->table[bucket->hash & (h->size - 1)]);
  bucket_t*  next = *head;

  *head = bucket;
  bucket->next = next;
  h->num ++;  
}

void
hashtbl_remove (hashtbl_t* h, bucket_t* bucket) {

  if( h->size ) {
    
    bucket_t** head_ptr = &(h->table[bucket->hash & (h->size - 1)]);
    bucket_t** next_ptr = head_ptr;

    /* Find the pointer that points at the bucket */
    while(*next_ptr != bucket) {
      /*assert(*next_ptr);*/
      next_ptr = (&(*next_ptr)->next);
    }
    
    /* Splice out the bucket */
    *next_ptr = (*next_ptr)->next;
    h->num --;
    
    if(h->num == 0) {
      /* This ensures that empty hashtables take no space. */
      hashtbl_resize(h, 0);
    }
    else if(h->num > CEAL_HASHTBL_INIT_SIZE &&
            (h->num <= h->size >> CEAL_HASHTBL_MINCAP_BITS)) {
      hashtbl_resize(h, h->size >> CEAL_HASHTBL_SHRINK_BITS);
    }
  }
  
}

bucket_t*
hashtbl_lookup (hashtbl_t* h, uintptr_t hash) {
  if(h->size)
    return h->table[hash & (h->size - 1)];
  else
    return NULL;
}
