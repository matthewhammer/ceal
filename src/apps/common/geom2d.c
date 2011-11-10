/* Matthew Hammer <hammer@tti-c.org> */

#ifndef __GEOM_2D_H__
#define __GEOM_2D_H__

#include <math.h>
#include <stdio.h>

#ifdef CEAL_FOREIGN_IMMUTABLES
#define GEOM2D_FOREIGN foreign_c
#else
#define GEOM2D_FOREIGN
#endif

typedef struct geom2d_point_s {
  double zwzr x;
  double zwzr y;
} geom2d_point_t;  

typedef struct geom2d_line_s {
  geom2d_point_t* zwzr p1;
  geom2d_point_t* zwzr p2;
} geom2d_line_t;

typedef struct geom2d_triangle_s {
  geom2d_point_t* zwzr p1;
  geom2d_point_t* zwzr p2;
  geom2d_point_t* zwzr p3;
} geom2d_triangle_t;


double             geom2d_cross(geom2d_point_t* p1, geom2d_point_t* p2);
double             geom2d_mag(geom2d_point_t* p);
void               geom2d_sub(geom2d_point_t* p1, geom2d_point_t* p2, geom2d_point_t* diff);
double             geom2d_dist(geom2d_point_t* p1, geom2d_point_t* p2);
long               geom2d_iszero(double f);
double             geom2d_line_point_distance(geom2d_line_t* line, geom2d_point_t* p);
void*              geom2d_signtest(double diff, void* a, void* b);
long               geom2d_isabove(geom2d_line_t* line, geom2d_point_t* p);
geom2d_point_t*    geom2d_toleft(geom2d_point_t* p1, geom2d_point_t* p2);
geom2d_point_t*    geom2d_toright(geom2d_point_t* p1, geom2d_point_t* p2);
geom2d_point_t*    geom2d_maxdist(geom2d_line_t* line, geom2d_point_t* p1, geom2d_point_t* p2);
void               geom2d_point_print(geom2d_point_t* p, FILE* f);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* 2D Points. */

static geom2d_point_t*
geom2d_point_init(geom2d_point_t* p, double x, double y) GEOM2D_FOREIGN {
  p->x = x;
  p->y = y;
  return p;
}

#define geom2d_point(x, y)                          \
  ({geom2d_point_init(alloc(geom2d_point_t),x,y);})

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* 2D Lines. */

static geom2d_line_t*
geom2d_line_init(geom2d_line_t* line,
                 geom2d_point_t* p1,
                 geom2d_point_t* p2) GEOM2D_FOREIGN {
  line->p1 = p1;
  line->p2 = p2;
  return line;
}

#define geom2d_line(p1, p2)                           \
  ({geom2d_line_init(alloc(geom2d_line_t), p1, p2);})

static geom2d_point_t*
geom2d_line_p1(geom2d_line_t* line) GEOM2D_FOREIGN {
  return line->p1;
}

static geom2d_point_t*
geom2d_line_p2(geom2d_line_t* line) GEOM2D_FOREIGN {
  return line->p2;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* 2D Geometry Primitives. */

static double
geom2d_cross(geom2d_point_t* p1,
             geom2d_point_t* p2) GEOM2D_FOREIGN {
  return (p1->x * p2->y) - (p1->y * p2->x);
}

static double
geom2d_mag(geom2d_point_t* p) GEOM2D_FOREIGN {
  return sqrt((p->x * p->x) + (p->y * p->y));
}

static void
geom2d_sub(geom2d_point_t* p1,
           geom2d_point_t* p2,
           geom2d_point_t* diff) GEOM2D_FOREIGN {
  diff->x = p1->x - p2->x;
  diff->y = p1->y - p2->y;
}

static double
geom2d_dist(geom2d_point_t* p1, geom2d_point_t* p2) GEOM2D_FOREIGN {
  return sqrt((p1->x - p2->x) * (p1->x - p2->x) +
              (p1->y - p2->y) * (p1->y - p2->y));
}

static long
geom2d_iszero(double f) GEOM2D_FOREIGN {
  return ((f >= -0.0001) &&
          (f <= +0.0001));
}

static double
geom2d_line_point_distance(geom2d_line_t* line,
                           geom2d_point_t* p) GEOM2D_FOREIGN {
  geom2d_point_t diff1;
  geom2d_point_t diff2;
  geom2d_point_t diff3;
  double numer;
  double denom;  

  geom2d_sub(line->p2, line->p1, &diff1);
  geom2d_sub(line->p1, p, &diff2);
  geom2d_sub(line->p2, line->p1, &diff3);

  numer = fabs(geom2d_cross(&diff1, &diff2));
  denom = geom2d_mag(&diff3);
  
  return numer / denom;
}

static void*
geom2d_signtest(double diff,
                void* a,
                void* b) GEOM2D_FOREIGN
{
  if(geom2d_iszero(diff))
    return a > b ? a : b;
  else if( diff < 0 )
    return a;
  else
    return b;
}

static long
geom2d_isabove(geom2d_line_t* line,
               geom2d_point_t* p) GEOM2D_FOREIGN {
  geom2d_point_t diff1;
  geom2d_point_t diff2;
  double cross;
  long result;
  geom2d_sub(line->p2, line->p1, &diff1);
  geom2d_sub(line->p1, p, &diff2);
  cross = geom2d_cross(&diff1, &diff2);
  if(geom2d_iszero(cross)) result = 0;
  else result = geom2d_signtest(cross, (void*) 1, (void*) 0);
  return result;
}

static geom2d_point_t*
geom2d_toleft(geom2d_point_t* p1,
              geom2d_point_t* p2) GEOM2D_FOREIGN {
  return geom2d_signtest(p1->x - p2->x, p1, p2);  
}

static geom2d_point_t*
geom2d_toright(geom2d_point_t* p1,
               geom2d_point_t* p2) GEOM2D_FOREIGN {
  return geom2d_signtest(p1->x - p2->x, p2, p1);
}

static geom2d_point_t*
geom2d_maxdist(geom2d_line_t* line, geom2d_point_t* p1, geom2d_point_t* p2) GEOM2D_FOREIGN {
  double d1 = geom2d_line_point_distance(line, p1);
  double d2 = geom2d_line_point_distance(line, p2);
  return geom2d_signtest(d2 - d1, p1, p2);
}

static void
geom2d_point_print(geom2d_point_t* p, FILE* f) GEOM2D_FOREIGN {
  fprintf(f, "(%.2g, %.2g)", p->x, p->y);  
}

#endif
