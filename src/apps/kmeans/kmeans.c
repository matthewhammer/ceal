#include <math.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include "../common/main.c"
#include <assert.h>
#include <string.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Globals store the inputs and outputs of the algorithm */

typedef struct point_s {
  double x;
  double y;
} point_t;

point_t* points;
long pointc;
long pointi;

point_t saved_point;

point_t* input_centroids; /* centroids picked to generate input. */
point_t* core_centroids;  /* self-adjusting output. */
point_t* verf_centroids;  /* conventional (verfifier) output. */


long centroidc = 5;

#define max_iterations 100


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


void* my_malloc(long size) {
  void* p = malloc(size);
  memset(p, 0, size);
  return p;
}

void kmeans(point_t* points, long pointc,
            point_t* centroids, long centroidc,
            long iterations) {
  
  if (iterations >= max_iterations) {
    return;
  }

  double *sumx = my_malloc(centroidc * sizeof(double));
  double *sumy = my_malloc(centroidc * sizeof(double));
  long   *count = my_malloc(centroidc * sizeof(long));

  for(long a = 0; a < centroidc; a++) {
    sumx[a] = 0;
    sumy[a] = 0;
    count[a] = 0;
  }
    
  for (long i = 0; i < pointc; i++) {
    cut {
      double mindist = DBL_MAX;
      long mincindex = -1;
      
      for (long j = 0; j < centroidc; j++) {
        double distance = sqrt(pow(points[i].x - centroids[j].x, 2) + 
                               pow(points[i].y - centroids[j].y, 2));
	
        if (distance < mindist) {
          mincindex = j;
          mindist = distance;
        }
      }
      
      assert(mincindex >= 0);
      
      sumx[mincindex] += points[i].x;
      sumy[mincindex] += points[i].y;
      count[mincindex]++;
    }
  }

  long unchanged = 1;

  for (long i = 0; i < centroidc; i++) {
    cut {
      double oldx = centroids[i].x;
      double oldy = centroids[i].y;
      centroids[i].x = sumx[i] / count[i];
      centroids[i].y = sumy[i] / count[i];
      
#if 0
      printf("Old Centroid (#%d): [%f:%f] New Centroid (#%d): [%f:%f]\n", 
             i, oldx, oldy, i, centroids[i].x, centroids[i].y);
#endif
      
      if(oldx != centroids[i].x || oldy != centroids[i].y) {
        unchanged = 0;
      }
    }
  }

  if (unchanged == 1) {
    return;
  }

  kmeans(points, pointc,
         centroids, centroidc,
         iterations+1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void dump_points(point_t* points, long pointc) {
  FILE* out = fopen("points.tmp", "w+");
  {long i; for(i = 0; i < pointc; i++) {
      fprintf(out, "%f %f\n", points[i].x, points[i].y);
    }}
  fclose(out);

  FILE* out = fopen("centroids.tmp", "w+");
  {long i; for(i = 0; i < centroidc; i++) {
      fprintf(out, "%f %f\n", core_centroids[i].x, core_centroids[i].y);
    }}
  fclose(out);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void cealtesthook_run_core() {
  core(kmeans)(points, pointc,
               core_centroids, centroidc, 0);
}
 
void cealtesthook_run_verf() {
  kmeans(points, pointc,
         verf_centroids, centroidc, 0);
}

void cealtesthook_input_generate(long size) {

  /* There mustn't be more centroids than points. */
  if(centroidc > size) {
    centroidc = size;
  }

  /* Allocate space to store the points and centroids. */
  pointc = size;
  points = my_malloc(sizeof(point_t) * pointc);

  input_centroids = my_malloc(sizeof(point_t) * centroidc);
  core_centroids  = my_malloc(sizeof(point_t) * centroidc);
  verf_centroids  = my_malloc(sizeof(point_t) * centroidc);
  
  /* diameter of each centroid's point cloud. */
  long diameter = 20;

  /* Divide the input points into clusters around each centroid. */
  long points_per_centroid = pointc / centroidc;
  long leftover_points = pointc % centroidc;

  /* Assign random point positions around the centroids. */
  for(long c = 0; c < centroidc; c++) {    

    /* pick a random position for the c^th centroid */
    input_centroids[c].x = rand() % 100;
    input_centroids[c].y = rand() % 100;

    /* Pick random initial positions for the output centroids. */
    verf_centroids[c].x = core_centroids[c].x = rand() % 100;
    verf_centroids[c].y = core_centroids[c].y = rand() % 100;
    
    /* Assign some points to the c^th centroid. */
    for(long i = points_per_centroid * c; 
        i < points_per_centroid * (c + 1); i++) {
      
      points[i].x = input_centroids[c].x - (diameter / 2) + (rand() % diameter);
      points[i].y = input_centroids[c].y - (diameter / 2) + (rand() % diameter);
    }
  }

  {
    /* Assign the remaining points to the centroids, in a round-robin fashion. */
    assert(leftover_points < centroidc);
    
    long j = points_per_centroid * centroidc;
    
    for(long c = 0; c < leftover_points; c++) {      
      points[j + c].x = input_centroids[c].x - (diameter / 2) + (rand() % diameter);
      points[j + c].y = input_centroids[c].y - (diameter / 2) + (rand() % diameter);
    }
  }
}

void cealtesthook_input_print(FILE* file) {
  dump_points(points, pointc);
}

void cealtesthook_input_iter_begin() {
  pointi = 0;
}

void cealtesthook_input_iter_next() {
  pointi++;
}

long  cealtesthook_input_iter_isdone() {
  return (!(pointi < pointc));
}

void cealtesthook_input_iter_change() {
  saved_point = points[pointi];
  points[pointi].x = rand() % 100;
  points[pointi].y = rand() % 100;
}

void cealtesthook_input_iter_revert() {
  points[pointi] = saved_point;
}

void cealtesthook_output_print(FILE* file, int v) {
  point_t* centroids = NULL;
  
  if     (v == 0) { centroids = core_centroids; }
  else if(v == 1) { centroids = verf_centroids; }
  else            { assert(0);                  }
  
  for(long c = 0; c < centroidc; c++) {
    fprintf(file, "(%f, %f) ", centroids[c].x, centroids[c].y);
  }
}

int cealtesthook_output_check() {
  for(long c = 0; c < centroidc; c++) {
    /* Check that the centroids were updated correctly, up to rounding
       errors for IEEE floating point arithmetic. */
    if( ( fabs( core_centroids[c].x - verf_centroids[c].x ) > 0.001 ) ||
        ( fabs( core_centroids[c].y - verf_centroids[c].y ) > 0.001 ) )
      return 0;
  }
  return 1;
}
