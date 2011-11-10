#ifndef __TEST_TIME_H__
#define __TEST_TIME_H__

#include <time.h>
#include <sys/time.h>
#include "test_types.h"

/* Wrappers for getting and manipulating the system time.  Refer to
   gettimeofday(2) and (struct timeval) from <time.h> and
   <sys/time.h> */

timeval_t*  time_save(timeval_t* time);
timeval_t*  time_diff(timeval_t* time_diff, timeval_t* time1, timeval_t* time2);
double      time_as_double(timeval_t* time);
char*       time_as_string(timeval_t* time);
int         time_elapsed_is_measurable(timeval_t* start);

#endif
