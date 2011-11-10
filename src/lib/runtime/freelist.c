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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "freelist.h"

#define MALLOC_OVERHEAD 16
#define ONEKILO 1024
#define PAGESIZE (((4 * ONEKILO) - MALLOC_OVERHEAD))

static freelist_node_t*
bump_node_ptr(freelist_node_t* node, uintptr_t nbytes) {
  assert(nbytes % (sizeof(void*)) == 0); /* Word-aligned bump */
  return (freelist_node_t*)
    ((void**) node) + (nbytes / sizeof(void*));
}

/* Push on one page-worth's of blocks */
static void
freelist_grow(freelist_t* freelist) {

  freelist_node_t* begin = malloc(PAGESIZE);
  freelist_node_t*   end = bump_node_ptr(begin, PAGESIZE);
  freelist_node_t*   cur = begin;
  freelist_node_t*  next = bump_node_ptr(cur, freelist->block_size);

  /* Are we out of memory? */
  if(!cur) {
    perror(__FUNCTION__);
    exit(EXIT_FAILURE);
  }

  /* Chain each block in the page together, stop when there is no
     space left in the page to which we can point cur->next. */
  while(bump_node_ptr(next, freelist->block_size) < end) {
    cur->next = next;
    cur = next;
    next = bump_node_ptr(next, freelist->block_size);
  }

  /* Push the entire page onto the list.  It's linked from begin to
     cur, in that direction. cur is the last block of the new page. */
  cur->next = freelist->head;
  freelist->head = begin;
}

void freelist_init(freelist_t* freelist, uintptr_t block_size) {
  assert(block_size >= sizeof(freelist_node_t));
  
  freelist->head = NULL;
  freelist->block_size = block_size;
}
                                    
/* Pop is analogous to "free".
 *
 * Pre: freelist->head is either NULL (implies that there are no free
 * blocks, so we grow) or freelist->head is non-NULL and we take this
 * head and move head to the next node.
 */
void* freelist_pop(freelist_t* freelist) {
  if(!freelist->head)
    freelist_grow(freelist);

  /* Now we have at least one node: */ {
    void* block = (void*) freelist->head;
    freelist->head = freelist->head->next;
    return block;
  }
}

/* Push is analogous to "malloc/new" */
void freelist_push(freelist_t* freelist, void* ptr) {
  freelist_node_t* node = (freelist_node_t*) ptr;
  node->next = freelist->head;
  freelist->head = node;
}
