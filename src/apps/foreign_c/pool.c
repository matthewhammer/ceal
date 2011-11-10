#ifndef __CEAL_POOL_C__
#define __CEAL_POOL_C__

#include <stdlib.h>

/*
  ceal_pool allocator.
  
  This allocation interface is useful when one wants to run some
  foreign_c code and this foreign code needs to do dynamic memory
  allocation.

  The question that often arises is, how shall this code clean up
  after itself?  If any core computation references these allocations,
  it is not safe to free them in the usual way (doing so will leave
  dangling pointers in the computation trace).

  Instead, the FFI for ceal_pool handles this cleanup automatically:
  when the ceal_pool_begin() function is revoked, it will free all the
  associated allocations, even if they occur in (untraced) foreign_c
  code.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Client Interface.
 */

struct ceal_pool_s;
typedef struct ceal_pool_s ceal_pool_t;

/* Create a new pool. */
ceal_pool_t* ceal_pool_begin() foreign_c;

/* Allocate something in the pool. */
/* This function can be called from foreign_c code even if the pool is
   created by the core program. */
void* ceal_pool_malloc( ceal_pool_t* pool, size_t size ) foreign_c;

/* Clean up a pool; reclaim and free all of its allocations. */
void ceal_pool_end( ceal_pool_t* pool ) foreign_c;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Implementation.
 */

#include "basemm.c"

/* A pool allocation. */
typedef struct ceal_pool_blkhdr_s {
  struct ceal_pool_blkhdr_s* next;
  size_t size;
  void* payload [0];
} ceal_pool_blkhdr_t;

/* A pool handle. */
struct ceal_pool_s {
  ceal_pool_blkhdr_t* blkhdrs;
};


static void
ceal_pool_free_all( ceal_pool_t* pool ) foreign_c {
  ceal_pool_blkhdr_t* blkhdr = pool->blkhdrs;

  while ( blkhdr ) {
    ceal_pool_blkhdr_t* next = blkhdr->next;
    printf("basemm_free %p\n", blkhdr->payload );
    basemm_free ( blkhdr->size, blkhdr );
    blkhdr = next;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/*  Pool_begin
 */

ceal_pool_t*
ceal_pool_begin() foreign_c {
  ceal_pool_t* pool = basemm_malloc( sizeof(ceal_pool_t) );
  pool->blkhdrs = NULL;
  return pool;
}

ceal_pool_t*
ceal_pool_begin_invoke( void* trnode, ceal_pool_t* pool ) foreign_c {
  pool->blkhdrs = NULL;
  return pool;
}

void
ceal_pool_begin_revoke( void* trnode, ceal_pool_t* pool ) foreign_c {
  ceal_pool_free_all( pool );
}

ceal_pool_t*
ceal_pool_begin_revinv( void* trnode, ceal_pool_t* pool ) foreign_c {
  ceal_pool_begin_revoke( trnode, pool );
  return ceal_pool_begin_invoke( trnode, pool );
}

#pragma CEAL_ffi(ceal_pool_begin, nondet)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/*  Pool_malloc
 */

/* Allocate something in the pool. */
void* ceal_pool_malloc( ceal_pool_t* pool, size_t size ) foreign_c {
  ceal_pool_blkhdr_t* blkhdr =
    basemm_malloc( sizeof(ceal_pool_blkhdr_t) + size );
  
  blkhdr->size = size ;
  blkhdr->next = pool->blkhdrs ;
  pool->blkhdrs = blkhdr;

  printf("basemm_malloc %p\n", blkhdr->payload );
  
  return blkhdr->payload;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/*  Pool_end
 */

void ceal_pool_end( ceal_pool_t* pool ) foreign_c {
  ceal_pool_free_all ( pool );
}

/* Nothing to do --- we rely on pool_begin_revoke to clean up. */
void ceal_pool_end_invoke( void* trnode, void* _, ceal_pool_t* pool ) foreign_c { }
void ceal_pool_end_revinv( void* trnode, void* _, ceal_pool_t* pool ) foreign_c { }
void ceal_pool_end_revoke( void* trnode, void* _ ) foreign_c { }


#pragma CEAL_ffi(ceal_pool_end)


#endif
