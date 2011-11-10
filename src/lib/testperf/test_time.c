#include <stdlib.h>
#include "test_time.h"
#include "test_utils.h"

timeval_t* time_save(timeval_t* time) {
  if(gettimeofday(time, NULL)) {
    perror("gettimeofday");
    exit(-1);
  }
  return time;
}

timeval_t* time_diff(timeval_t* time_diff, timeval_t* time1, timeval_t* time2) {
  time_diff->tv_sec  = time1->tv_sec  - time2->tv_sec;
  time_diff->tv_usec = time1->tv_usec - time2->tv_usec;
  return time_diff;
}

double time_as_double(timeval_t* time) {
  return ((double) time->tv_sec +
          ((double) time->tv_usec / (double) 1000000));
}

char* time_as_string(timeval_t* time) {
  double d = time_as_double(time);
  if(d < 100.0) {
    return cheap_sprintf("%g sec", d);
  }
  else {
    int    min = ((int) d) / 60;
    double sec = d - (60 * min);

    if (d < (100.0 * 60.0)) {
      return cheap_sprintf("%ld min %.1f sec", min, sec);
    }
    else {
      int hrs = min / 60;
      min = min - (60 * hrs);

      return cheap_sprintf("%ld hrs %ld min %.1f sec", hrs, min, sec);
    }        
  }
}

int time_elapsed_is_measurable(timeval_t* start) {
  timeval_t now;
  time_save(&now);    
  return
    ((now.tv_sec  - start->tv_sec)  > 0      ) ||
    ((now.tv_usec - start->tv_usec) > 100000 ) ;
}
