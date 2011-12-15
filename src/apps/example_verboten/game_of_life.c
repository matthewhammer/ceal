/* Matthew Hammer <hammer@mpi-sws.org> */

#include <stdlib.h>
#include <string.h>
#include "cealtesthook_dummies.c"

typedef enum { DEAD, ALIVE } state_t;

typedef struct cell_s {
  long curr_state;
  long prev_state;
} cell_t;

typedef struct world_s {
  cell_t* zwzr cells;
  long    zwzr width;
  long    zwzr height;
} world_t;

long db_flag = 0;
long clear_flag = 0;

#define db_printf(x,...) if(db_flag){ printf(x,##__VA_ARGS__); }

world_t* world_alloc(long width, long height) {
  world_t* w = alloc(world_t);
  w->width  = width;
  w->height = height;
#if 1
  w->cells  = (cell_t*) malloc(width * height * sizeof(cell_t));
  memset((void*)w->cells, 0, width * height * sizeof(cell_t));
#else
  m->cells  = alloc( cell_t [width * height] );
#endif
  return w;
}

long world_idx(world_t* world, long col, long row) {
  while( col < 0 ) { col += world->width; }
  while( row < 0 ) { row += world->height; }  
  return world->width * (row % world->height) + (col % world->width);
}

inline long cell_get_prev(world_t* world, long col, long row) {
  return world->cells[ world_idx( world, col, row) ].prev_state;
}

inline long cell_get_curr(world_t* world, long col, long row) {
  return world->cells[ world_idx( world, col, row) ].curr_state;
}

inline void cell_set_curr(world_t* world, long col, long row, long x) {
  world->cells[ world_idx( world, col, row ) ].curr_state = x;
}

inline void cell_save(world_t* world, long col, long row) {
  cell_t* cell = & world->cells[ world_idx( world, col, row ) ];
  long curr_state  = cell->curr_state;
  cell->prev_state = curr_state;
}

void cell_step (world_t* world, long c, long r) {
  long neighbors_alive  =
    (cell_get_prev(world, c-1, r-1) == ALIVE) +
    (cell_get_prev(world, c-1, r+1) == ALIVE) +
    (cell_get_prev(world, c+1, r-1) == ALIVE) +
    (cell_get_prev(world, c+1, r+1) == ALIVE) +
    (cell_get_prev(world, c  , r-1) == ALIVE) +
    (cell_get_prev(world, c  , r+1) == ALIVE) +
    (cell_get_prev(world, c-1, r  ) == ALIVE) +
    (cell_get_prev(world, c+1, r  ) == ALIVE) ;

  db_printf("neighbors_alive(%d,%d) = %d\n", c, r, neighbors_alive);
  
  if( cell_get_prev(world, c,r) == ALIVE ) {
    /* prev state was ALIVE */

    if( neighbors_alive < 2 )
      /* Too few neighbors to survive. */
      cell_set_curr(world, c, r, DEAD);
    
    else if( neighbors_alive <= 3 )
      /* Just right: survive. */
      cell_set_curr(world, c, r, ALIVE);
    
    else
      /* Too many neighbors, so die. */
      cell_set_curr(world, c, r, DEAD);
  }
  else {
    /* prev state was DEAD */
    
    if( neighbors_alive == 3 )
      /* Neighbors make cell alive via "reproduction". */
      cell_set_curr(world, c, r, ALIVE);
    else
      cell_set_curr(world, c, r, DEAD);
  }
}

#define WORLD_ITER(world,c,r,body)              \
  for(long r = 0; r < world->height; r++) {     \
    for(long c = 0; c < world->width; c++) {    \
      body;                                     \
    }                                           \
  }                                             \

void world_dead(world_t* world) {
  WORLD_ITER(world,c,r,{
      cell_set_curr(world,c,r,DEAD);
    });
}

void world_rand(world_t* world, long rand_mod) {
  WORLD_ITER(world,c,r,{
      if(rand() % rand_mod == 0)
        cell_set_curr(world, c, r, ALIVE);
      else
        cell_set_curr(world, c, r, DEAD);
    });
}

void world_save(world_t* world) {
  WORLD_ITER(world,c,r, cut{cell_save(world, c, r);} );
}

void world_step(world_t* world) {
  WORLD_ITER(world,c,r, cut{cell_step(world, c, r);} );
  WORLD_ITER(world,c,r,
             cut{
               db_printf("(%d,%d) is %d\n", c, r,
                         cell_get_curr(world,c,r) );
             } );
}

long draw_count = 0;
void world_draw_whole(world_t* world) {

#define EOL "\x1b[K\n"

  long __dummy = draw_count;
  
  if( clear_flag ) {
    /* vt100 : move cursor to home */
    printf("\x1b[H");
  }
  
  printf(" draw %d begin" EOL, draw_count );
 
  WORLD_ITER(world,c,r, cut {
      long curr_state = cell_get_curr(world,c,r);
      long changed = cut((long)(cell_get_prev(world,c,r) != curr_state));

      if(c == 0)
        printf(" |");
      
      if(changed) {
        if(curr_state == ALIVE) {
          /* vt100 : green */
          printf("\x1b[1;32m" "#" "\x1b[0m");
        }
        else {
          /* vt100 : red */
          printf("\x1b[31m" "-" "\x1b[0m");
        }
      }
      else {
        printf("%s", curr_state == ALIVE ? "#" : " ");
      }

      if(c == world->width - 1)
        printf("|" EOL);
    });

  printf(" draw %d end" EOL "\x1b[J", draw_count );
  draw_count ++;
}

void world_draw( world_t* world, uintptr_t do_full ) {

  printf("\x1b[2J");
  
  WORLD_ITER(world,c,r, cut {
      long curr_state = cell_get_curr(world,c,r);
      long changed = cut((long)(cell_get_prev(world,c,r) != curr_state));
      
      if( changed ) {
        /* Move the cursor. */
        printf("\x1b[%d;%dH",r,c);

        if( curr_state == ALIVE ) {
          /* vt100 : green */
          /* printf("\x1b[1;32m" "#" "\x1b[0m"); */
          printf("#");
        }
        else {
          /* vt100 : red */
          /* printf("\x1b[31m" "-" "\x1b[0m"); */
          printf(" ");
        }
      }
      else if( do_full ) {
        /* Move the cursor. */
        printf("\x1b[%d;%dH",r,c);
        printf("%s", curr_state == ALIVE ? "#" : " ");
      }
    });  
}

void world_loop_body(world_t* world) {
  world_save( world );
  world_step( world );
  world_draw( world, 0 );
}

/* If feedback is working correctly, the behavior with CP should be
   equivalent to that without CP. */
int main(int argc, char* foreign_c* argv) {

  int use_cp = 1;
  long size = 5;
  long loops = 10;
  long rand_mod = 4;
  
#define long_arg(var,name)                                      \
  if( i + 1  < argc ) {                                         \
    var = atol( argv[ ++i ] );                                  \
  }                                                             \
  else {                                                        \
    fprintf(stderr, name ": expected an argument.\n");          \
    exit( -1 );                                                 \
  }
  
  /* process the arguments. */
  for(int i = 0; i < argc; i++) {
    if(!strcmp(argv[i], "-nocp")) {
      use_cp = 0;
    }
    else if(!strcmp(argv[i], "-db")) {
      db_flag = 1;
    }
    else if(!strcmp(argv[i], "-clear")) {
      clear_flag = 1;
    }    
    else if(!strcmp(argv[i], "-size")) {
      long_arg(size, "-size");
    }
    else if(!strcmp(argv[i], "-loops")) {
      long_arg(loops, "-loops");
    }
    else if(!strcmp(argv[i], "-rand-mod")) {
      long_arg(rand_mod, "-rand-mod");
    }
  }
      
  world_t* world = world_alloc( size, size );
  world_rand( world, rand_mod );
  world_save( world );
  world_draw( world, 1 );

  if( use_cp ) {
    if(loops) {
      db_printf("step begin\n");
      core(world_loop_body)( world );
      db_printf("step end\n");
    }
    for(int i = 0; loops > 1 && i < loops - 1; i++) {
      db_printf("step begin\n");
      propagate;
      db_printf("step end\n");
    }
  }
  else {
    for(int i = 0; i < loops; i++) {
      db_printf("step begin\n");
      world_loop_body( world );
      db_printf("step end\n");
    }
  }
    
  return 0;
}
