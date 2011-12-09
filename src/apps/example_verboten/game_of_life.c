/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdlib.h>
#include "cealtesthook_dummies.c"

typedef enum { DEAD, ALIVE } state_t;

typedef struct cell_s {
  long feedback curr_state;
  long          prev_state;
} cell_t;

typedef struct world_s {
  cell_t* zwzr cells;
  long zwzr width;
  long zrzr height;
} world_t;

void world_alloc(long width, long height) {
  world_t* w = alloc(world_t);
  w->width  = width;
  w->height = height;
#if 1
  w->cells  = malloc(width * height * sizeof(cell_t));
  memset(w->cells, 0, width * height * sizeof(cell_t));
#else
  m->cells  = alloc( cell_t [width * height] );
#endif
  return w;
}

inline long world_idx(world_t* world, long col, long row) {
  return world->width * (row % world->height) + (col % world->width);
}

inline cell_t cell_get_prev(world_t* world, long col, long row) {
  return world->cells[ world_idx( world, col, row) ].prev_state;
}

inline void cell_set_curr(world_t* world, long col, long row, cell_t c) {
  world->cells[ world_idx( world, col, row ) ].curr_state = c;
}

inline void cell_save(world_t* world, long col, long row, cell_t c) {
  cell_t* cell = & world->cells[ world_idx( world, col, row ) ];
  cell->prev_state = (long) cell->curr_state;
}

inline void cell_step (world_t* world, long c, long r) {
  long neighbors_alive  =
    (cell_get_prev(c-1, r-1) == ALIVE) +
    (cell_get_prev(c-1, r+1) == ALIVE) +
    (cell_get_prev(c+1, r-1) == ALIVE) +
    (cell_get_prev(c+1, r+1) == ALIVE) +
    (cell_get_prev(c  , r-1) == ALIVE) +
    (cell_get_prev(c  , r+1) == ALIVE) +
    (cell_get_prev(c-1, r  ) == ALIVE) +
    (cell_get_prev(c+1, r  ) == ALIVE) ;
  
  if( cell_get_prev(c,r) == ALIVE ) {
    /* prev state was ALIVE */

    if( neighbors_alive < 2 )
      /* Too few neighbors to survive. */
      cell_set_curr(c, r, DEAD);
    
    else if( neighbors_alive <= 3 )
      /* Just right: survive. */
      cell_set_curr(c, r, ALIVE);
    
    else
      /* Too many neighbors, so die. */
      cell_set_curr(c, r, DEAD);
  }
  else /* prev state was DEAD */
    if( neighbors_alive == 3 )
      /* Neighbors make cell alive via "reproduction". */
      cell_set_curr(c, r, ALIVE);
}

void world_dead(world_t* world) {
  for(long w = 0; w < world->width; w++) {
    for(long h = 0; h < world->height; h++) {
      cell_set_next(w, h, DEAD);
    }
  }
}

void world_rand(world_t* world) {
  for(long c = 0; w < world->width; w++) {
    for(long r = 0; h < world->height; h++) {
      if(rand() % 5)
        world_set_next(world, c, r, ALIVE);
      else
        world_set_next(world, c, r, DEAD);
    }
  }
}

void world_step(world_t* world) {
  /* Compute the next state for each cell. */
  for(long c = 0; w < world->width; w++) {
    for(long r = 0; h < world->height; h++) {
      cut {
        cell_save(world, c, r);
      }
    }
  }
  /* Commit the next state as the current one. */
  for(long c = 0; w < world->width; w++) {
    for(long r = 0; h < world->height; h++) {
      cut {
        cell_step(world, c, r);
      }
    }
  }  
}
