/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * POWER MEASUREMENT STUFF.
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */

#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "test_types.h"
#include "test_time.h"
#include "trace.h"

void print_basemm_stats(test_state_t* test_state, const char* phase);
void ceal_propagate();

static void
testpower_begin(test_state_t* test_state) {

  fprintf(stderr, "%s: sleeping for %d seconds ...\n",
          __FUNCTION__, test_state->params.testpower.initsleep );

  sleep ( test_state->params.testpower.initsleep );

  fprintf(stderr, "%s: starting experiment: %s\n",
          __FUNCTION__, test_state->params.testpower.experiment );

  time_save(& test_state->stats_testpower.time_begin );
}

static void
testpower_print_wattsup_fmt (const char* label, timeval_t* timeval) {  

  struct tm* tm = localtime( & timeval->tv_sec );

  printf("%s: [%02d:%02d:%02d.%03d]\n", label,
         tm->tm_hour, tm->tm_min, tm->tm_sec,
         (int) ((double) timeval->tv_usec / 1000) );
}

static void
testpower_end( test_state_t* test_state, long num_operations ) {

  time_save( & test_state->stats_testpower.time_end );
  
  time_diff( & test_state->stats_testpower.time_delta,
             & test_state->stats_testpower.time_end,
             & test_state->stats_testpower.time_begin );

  printf("== BEGIN: testpower output ==\n");
  
  /* TODO -- This is an inaccurate/misleading output message. */
  printf("Array size in bytes: %i\n", test_state->params.input_size );

  printf("Total ops: %ld\n", num_operations );
  
  testpower_print_wattsup_fmt( "Start time",
                               & test_state->stats_testpower.time_begin );

  testpower_print_wattsup_fmt( "Finish time",
                               & test_state->stats_testpower.time_end );

  printf("Run time [us]: %ld\n",
         (long) ( time_as_double( & test_state->stats_testpower.time_delta ) * 1000000L ) );

  printf("== END: testpower output ==\n");
    
  print_basemm_stats( test_state, "testpower" );
  
}

typedef enum {
  EXPERIMENT_VERF,
  EXPERIMENT_CP,
} experiment_t;

static void
testpower_do_initial(test_state_t* test_state, experiment_t experiment)
{
  if ( experiment == EXPERIMENT_VERF ) {
    cealtesthook_run_verf();
  }
  else if ( experiment == EXPERIMENT_CP ) {
    cealtesthook_run_core();
  }
  else {
    abort();
  }  
}

static void
testpower_do_update(test_state_t* test_state, experiment_t experiment)
{
  if ( experiment == EXPERIMENT_VERF ) {        
    /* Track all the allocations; free them afterward. */
    ceal_alloch_t* metaboxes0 = ceal_metaboxes_new();

    cealtesthook_run_verf();
    
    ceal_metaboxes_free ( ceal_metaboxes_get() );
    ceal_metaboxes_set ( metaboxes0 );    
  }
  else if ( experiment == EXPERIMENT_CP ) {
    ceal_propagate();
  }
  else {
    abort();
  }
}

static  void
testpower_loop(test_state_t* test_state, experiment_t experiment) {
  long loop_count  = 0;
  int change_count = 0;
  double elapsed;

  fprintf(stderr, "%s: beginning initial run.\n",  __FUNCTION__);

  testpower_do_initial( test_state, experiment );

  fprintf(stderr, "%s: initial run complete.\n",  __FUNCTION__);
  fprintf(stderr, "%s: beginning changes.\n",  __FUNCTION__);

  cealtesthook_input_iter_begin( test_state->params.change_size );
  
  testpower_begin( test_state );

  do {    
    cealtesthook_input_iter_change();
    testpower_do_update( test_state, experiment );

    cealtesthook_input_iter_revert();
    testpower_do_update( test_state, experiment );

    loop_count   += 2;
    change_count += 1;

    { /* Iterate. */      
      cealtesthook_input_iter_next();
      if( cealtesthook_input_iter_isdone() ) {
        cealtesthook_input_iter_begin( test_state->params.change_size );
      }         
    }
    
    time_save ( & test_state->stats_testpower.time_end );
    time_diff ( & test_state->stats_testpower.time_delta,
                & test_state->stats_testpower.time_end,
                & test_state->stats_testpower.time_begin );

    elapsed = time_as_double( & test_state->stats_testpower.time_delta );
  }
  while ( elapsed * 1000000L < test_state->params.testpower.runtime ) ;

  testpower_end ( test_state, loop_count );  
}


void
testpower_main( test_state_t* test_state ) {

  fprintf(stderr, "%s: input_generate: begin ...\n",  __FUNCTION__);
  cealtesthook_input_generate(test_state->params.input_size);
  fprintf(stderr, "%s: input_generate: done.\n",  __FUNCTION__);
  
  if( !strcmp( test_state->params.testpower.experiment, "verf" ) ) {
    assert( (ceal_init_flags() & CEAL_INIT_VERIFIER)
            && "(argument `-testpower verf' does not apply here)." );
    testpower_loop( test_state, EXPERIMENT_VERF );
  }
  else if ( !strcmp( test_state->params.testpower.experiment, "cp" ) ) {
    assert( (ceal_init_flags() & CEAL_INIT_SELFADJ)
            && "(argument `-testpower cp' does not apply here)." );
    testpower_loop( test_state, EXPERIMENT_CP );
  }
  else {
    fprintf(stderr, "don't know what experiment this is: %s\n",
            test_state->params.testpower.experiment);
  }  
}
