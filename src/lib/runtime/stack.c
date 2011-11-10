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

/* Matthew Hammer <hammer@tti-c.org> */

#include <stdlib.h>
#include "stack.h"

static void moveto_allempty(ceal_stack_t* stack) {
  ceal_stack_chunk_t* chunk = stack->nonempty;
  stack->nonempty = chunk->hdr.tail;
  chunk->hdr.tail = stack->allempty;
  stack->allempty = chunk;
}

static void moveto_nonempty(ceal_stack_t* stack) {
  ceal_stack_chunk_t* chunk = stack->allempty;

  if(chunk) {
    stack->allempty = chunk->hdr.tail;
  }
  else {
    chunk = malloc(sizeof(ceal_stack_chunk_t));
    chunk->hdr.num = 0;
  }
  
  chunk->hdr.tail = stack->nonempty;
  stack->nonempty = chunk;
}


ceal_stack_t* ceal_stack_init(ceal_stack_t* stack) {
  stack->nonempty = NULL;
  stack->allempty = NULL;
  return stack;
}

void* ceal_stack_push(ceal_stack_t* stack, void* item) {
  ceal_stack_chunk_t* chunk = stack->nonempty;

  if(!chunk || chunk->hdr.num == CHUNK_SIZE) {
    moveto_nonempty(stack);
    return ceal_stack_push(stack, item);
  }
  else {  
    chunk->items[chunk->hdr.num ++] = item;
    return item;
  }
}

void* ceal_stack_pop(ceal_stack_t* stack) {
  ceal_stack_chunk_t* chunk = stack->nonempty;
  
  if(chunk) {  
    if(chunk->hdr.num == 0) {
      moveto_allempty(stack);
      return ceal_stack_pop(stack);
    }
    else {
      return chunk->items[-- chunk->hdr.num];
    }
  }
  else {
    return NULL;
  }
}

void* ceal_stack_peek(ceal_stack_t* stack) {
  ceal_stack_chunk_t* chunk = stack->nonempty;
  
  if(chunk) {  
    if(chunk->hdr.num == 0) {
      moveto_allempty(stack);
      return ceal_stack_peek(stack);
    }
    else {
      return chunk->items[chunk->hdr.num - 1];
    }
  }
  else {
    return NULL;
  }
}

int ceal_stack_isempty(ceal_stack_t* stack) {
  ceal_stack_chunk_t* chunk = stack->nonempty;
  
  return ( ( chunk == NULL )
           || ( chunk->hdr.num == 0 ) );
}
