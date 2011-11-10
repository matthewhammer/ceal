#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#include "test_types.h"
#include "test_utils.h"
#include "test_params.h"
#include "test_time.h"

#include "basemm.h"
#include "state.h"
#include "logging.h"

#define MINUS_SEP_08 "- - - - - "  "- - - "
#define MINUS_SEP_10 "- - - - - "  "- - - - - "  
#define MINUS_SEP_20 MINUS_SEP_10  MINUS_SEP_10
#define MINUS_SEP_80 MINUS_SEP_20  MINUS_SEP_20  MINUS_SEP_20  MINUS_SEP_20

#define PLUS_SEP_08 "+ + + + + "  "+ + + "
#define PLUS_SEP_10 "+ + + + + "  "+ + + + + "
#define PLUS_SEP_20 PLUS_SEP_10   PLUS_SEP_10
#define PLUS_SEP_80 PLUS_SEP_20   PLUS_SEP_20   PLUS_SEP_20   PLUS_SEP_20

static FILE*
inout_file_open(test_state_t* test_state, const char* label) {  
  int use_inout_files = test_flagval(test_state->params.inout_files);
  static int ext = 0;
    
  if(use_inout_files) {
    return fopen(cheap_sprintf("%s.%08d", label, ext++), "w+");
  }
  else {
    fprintf(stderr, " %6s = ", label);
    return stderr;
  }  
}

static void
inout_file_close(test_state_t* test_state, FILE* file) {
  int use_inout_files = test_flagval(test_state->params.inout_files);
  if(use_inout_files) {
    fclose(file);
  }
  else {
    fflush(file);
  }
}

static void
print_input(test_state_t* test_state) {
  if(test_flagval(test_state->params.print_inout)) {
    FILE* file = inout_file_open(test_state, "input");
    cealtesthook_input_print(file);
    fprintf(file, "\n");
    inout_file_close(test_state, file);
  }
}

static void
print_output(test_state_t* test_state, cealtesthook_output_version_t ver) {
  if(test_flagval(test_state->params.print_inout)) {
    FILE* file = inout_file_open(test_state, "output");
    cealtesthook_output_print(file, ver);
    fprintf(file, "\n");
    inout_file_close(test_state, file);
  }
}

static void
check_last_update_against_verifier(test_state_t* test_state) {
  if(test_flagval(test_state->params.verf_all_updates)) {
    int this_run = -1;
    { static int runs = 0; this_run = runs; runs += 1; }
    
    logg(MINUS_SEP_20 "Post-update Verifier run %d ("PHASE_ALLINSREM") " MINUS_SEP_20,
         this_run);
    
    cealtesthook_run_verf();

    logg(PLUS_SEP_20 "Post-update Verifier run %d ("PHASE_ALLINSREM") " PLUS_SEP_20,
         this_run);
    
    logg(MINUS_SEP_20 "Post-update check %d ("PHASE_ALLINSREM") " MINUS_SEP_20,
         this_run);

    int result_check = cealtesthook_output_check();
        
    if(! result_check) {
      logg("check FAIL.");
      fprintf(stderr, "%s: verifier does not agree (on run %d).\n",
              __FUNCTION__, this_run);
      test_state->exit_status = EXIT_FAILURE;
    }
    else {
      logg("check passed.");
    }
    
    if(test_flagval(test_state->params.print_inout)) {
      FILE* file = inout_file_open(test_state, "check");
      if( result_check ) {
        fprintf(file, "passed.\n");
      }
      else {
        fprintf(file, "FAIL.\n");
      }
      inout_file_close(test_state, file);
    }
    
    logg(PLUS_SEP_20 "Post-update check %d ("PHASE_ALLINSREM") " PLUS_SEP_20,
         this_run);
  }
}

static int
allinsrem(test_state_t* test_state) {
  int change_count   = 0;

  /* Begin iterating over the input. */
  cealtesthook_input_iter_begin( test_state->params.change_size );
  
  /* -- For each position in this input: */
  while(! cealtesthook_input_iter_isdone()) {
    int change_or_revert;

    /* -- Should we do a change? Or not? */
    if(test_state->params.change_fraction == 1.0 ||
       ((test_state->params.change_fraction * RAND_MAX) > rand())) {

      /* -- Change, then revert. */
      for(change_or_revert = 0; change_or_revert < 2; change_or_revert++) {

        change_count += 1;

        /* -- Change / Revert */
        if(change_or_revert == 0) {
          cealtesthook_input_iter_change();
        }
        else if(change_or_revert == 1) {
          cealtesthook_input_iter_revert();
        }
        else {
          abort();
        }

        /* -- Print changed/reverted input. */
        print_input(test_state);

        /* -- Propagate. */
        ceal_propagate();
                
        /* -- Print changed/reverted output. */
        print_output(test_state, OUTPUT_OF_CORE);
        
        /* -- Check the output against the verifier. */
        check_last_update_against_verifier(test_state);
      }
    }

    if(cealtesthook_input_iter_isdone()) {
      /* -- No subsequent input positions */
      break;
    }
    else {
      /* -- Next input position. */
      cealtesthook_input_iter_next();
      continue;
    }
  }
  
  return change_count;
}


static int
allinsrem_fromscratch(test_state_t* test_state) {
  int change_count   = 0;

  /* Begin iterating over the input. */
  cealtesthook_input_iter_begin( test_state->params.change_size );
  
  /* -- For each position in this input: */
  while(! cealtesthook_input_iter_isdone()) {
    int change_or_revert;

    /* -- Should we do a change? Or not? */
    if(test_state->params.change_fraction == 1.0 ||
       ((test_state->params.change_fraction * RAND_MAX) > rand())) {

      /* -- Change, then revert. */
      for(change_or_revert = 0; change_or_revert < 2; change_or_revert++) {

        change_count += 1;

        /* -- Change / Revert */
        if(change_or_revert == 0) {
          cealtesthook_input_iter_change();
        }
        else if(change_or_revert == 1) {
          cealtesthook_input_iter_revert();
        }
        else {
          abort();
        }

        /* -- Print changed/reverted input. */
        print_input(test_state);
        
        /* -- Run the (self-adjusting) application */
        ceal_core_reset();
        cealtesthook_run_core();
                
        /* -- Print changed/reverted output. */
        print_output(test_state, OUTPUT_OF_CORE);
        
        /* -- Check the output against the verifier. */
        check_last_update_against_verifier(test_state);
      }
    }

    if(cealtesthook_input_iter_isdone()) {
      /* -- No subsequent input positions */
      break;
    }
    else {
      /* -- Next input position. */
      cealtesthook_input_iter_next();
      continue;
    }
  }
  
  return change_count;
}


static int
verifier(test_state_t* test_state) {
  int runs = 0;
  timeval_t start;

  time_save(&start);

  while( ! time_elapsed_is_measurable(&start) )
  {
    logg(MINUS_SEP_20 "Verifier run %d ("PHASE_VERIFIER") " MINUS_SEP_20, runs);
    cealtesthook_run_verf();
    logg(PLUS_SEP_20 "Verifier run %d ("PHASE_VERIFIER") " PLUS_SEP_20, runs);
    runs += 1;

    if( ! test_flagval( test_state->params.loop_verf ) )
      break;
  }

  return runs;
}

static void
print_key_value(test_state_t* test_state,
                const char* phase,
                const char* key,
                const char* fmt, ...) {
  { va_list ap;
    va_start(ap, fmt);
    fprintf(stdout, ">> ");
    
    if(strlen(test_state->params.prefix))
      fprintf(stdout, "%s/", test_state->params.prefix);
    
    if(strlen(phase))
      fprintf(stdout, "%s/", phase);
    
    fprintf(stdout, "%s: ", key);
    vfprintf(stdout, fmt, ap);
    fprintf(stdout, "\n");
    va_end(ap);
  }
}

void
print_basemm_stats(test_state_t* test_state,
                   const char* phase) {

  print_key_value(test_state, phase,
                  "all-malloc-bytes", "%llu (%s)",
                  (unsigned long long) basemm_bytes_malloc,
                  string_of_size(2, basemm_bytes_malloc));
  
  print_key_value(test_state, phase,
                  "all-free-bytes", "%llu (%s)",
                  (unsigned long long) basemm_bytes_free,
                  string_of_size(2, basemm_bytes_free));
  
  print_key_value(test_state, phase,
                  "all-maxlive-bytes", "%llu (%s)",
                  (unsigned long long) basemm_bytes_maxlive,
                  string_of_size(2, basemm_bytes_maxlive));
}

static void
print_analytic_stats(test_state_t* test_state,
                     const char* phase) {
  
#if CEAL_ANALYTIC_STATS
  struct entry_s { const char* name; long* val; };

  struct ceal_analytic_stats_s* stats0 =
    &ceal_state->analytic_stats__from_scratch;

  assert ( stats0->trnode_free == 0 );
  
  struct ceal_analytic_stats_s* stats =
    &ceal_state->analytic_stats;
    
  struct entry_s entries[] = {
    { "propagations",  & stats->propagations },
    { "trnode-initial",& stats0->trnode_new },
    { "trnode-new",    & stats->trnode_new },
    { "trnode-undo",   & stats->trnode_undo },
    { "trnode-free",   & stats->trnode_free },
    { "trnode-redo",   & stats->trnode_redo },

    { "read-invoke",   & stats->read_invoke },
    { "read-revinv",   & stats->read_revinv },
    { "read-revoke",   & stats->read_revoke },

    { "write-invoke",  & stats->write_invoke },
    { "write-revinv",  & stats->write_revinv },
    { "write-revoke",  & stats->write_revoke },

    { "memo-invoke",   & stats->memo_invoke },
    { "memo-hit",      & stats->memo_hit },
    { "memo-miss",     & stats->memo_miss },
    { "memo-revoke",   & stats->memo_revoke },
    { "memo-move",     & stats->memo_move },
    { "memo-search",   & stats->memo_search },
  };
    
  int entries_count = sizeof(entries) / sizeof(struct entry_s);

  { int i;
    for(i = 0; i < entries_count; i++) {
      print_key_value(test_state, phase,
                      entries[i].name, "%llu (%s)",
                      (unsigned long long) *entries[i].val,
                      string_of_size(10, *entries[i].val));
    }
  }
#endif
  
}

static void
print_desc_stats(test_state_t* test_state,
                 const char* phase) {
#if CEAL_ANALYTIC_STATS
  if( test_flagval( test_state->params.print_desc_stats ) ) {
    ceal_desc_t* desc = ceal_state->descs;
  
    while(desc->next != NULL) {
      ceal_desc_stats_t* stats = NULL;
      
      if(ceal_state->phase == CEAL_CORE_COMPLETE) {
        stats = desc->stats_fromscratch;
      }
      else if(ceal_state->phase == CEAL_PROPAGATION_COMPLETE) {
        stats = desc->stats_propagation;
      }
      
      if(! stats ) goto next;
      
      fprintf(stdout, "== %s: "
              "new=%8ld | redo=%8ld | undo=%8ld | free=%8ld "
              "| %6ld bytes/e | %10ld bytes/t | %s:%ld\n",
              desc->name,
              stats->new, stats->redo, stats->undo, stats->free,
              desc->size,
              desc->size * (stats->new - stats->free),
              desc->file, desc->line
              );
    next:
      desc = desc->next;
    }
  }
#endif
}

static void
phase_fromscratch(test_state_t* test_state) {
  const char* phase = "fromscratch";
  test_stats_t* stats = &test_state->stats_fromscratch;
  fprintf(stderr, "%s: beginning from-scratch run.\n",  __FUNCTION__);
  time_save(&stats->time_begin);

  /* -- Run the (self-adjusting) application */
  cealtesthook_run_core();
  
  time_save(&stats->time_end);
  time_diff(&stats->time_delta, &stats->time_end, &stats->time_begin);
  fprintf(stderr, "%s: from-scratch run complete.\n",  __FUNCTION__);

  /* -- Print output. */
  print_output(test_state, OUTPUT_OF_CORE);

  print_key_value(test_state, phase, "time", "%g (%s)",
                  time_as_double(&stats->time_delta),
                  time_as_string(&stats->time_delta));         
  
  print_basemm_stats(test_state, phase);
  print_analytic_stats(test_state, phase);
  print_desc_stats(test_state, phase);
}

static void
phase_allinsrem(test_state_t* test_state) {
  const char* phase = "allinsrem";
  long divisor;
  test_stats_t* stats = &test_state->stats_changeall;
    
  fprintf(stderr, "%s: beginning changes.\n",  __FUNCTION__);
  time_save(&stats->time_begin);
  
  divisor = allinsrem(test_state);
  
  time_save(&stats->time_end);
  time_diff(&stats->time_delta, &stats->time_end, &stats->time_begin);
  fprintf(stderr, "%s: all changes complete.\n", __FUNCTION__);
  
  print_key_value(test_state, phase, "time-sum", 
                  "%g (%s for %d updates)",
                  time_as_double(&stats->time_delta),
                  time_as_string(&stats->time_delta),
                  divisor);

  print_key_value(test_state, phase, "time-ave",
                  "%g (ave over %d updates)",
                  time_as_double(&stats->time_delta) / ((double)divisor),
                  divisor);

  /*test_stats_print(stats, prefix, divisor);*/
  print_basemm_stats(test_state, phase);
  print_analytic_stats(test_state, phase);
  print_desc_stats(test_state, phase);
}


static void
phase_allinsrem_fromscratch(test_state_t* test_state) {
  const char* phase = "allinsrem_fromscratch";
  long divisor;
  test_stats_t* stats = &test_state->stats_changeall;
    
  fprintf(stderr, "%s: beginning changes.\n",  __FUNCTION__);
  time_save(&stats->time_begin);
  
  divisor = allinsrem_fromscratch(test_state);
  
  time_save(&stats->time_end);
  time_diff(&stats->time_delta, &stats->time_end, &stats->time_begin);
  fprintf(stderr, "%s: all changes complete.\n", __FUNCTION__);
  
  print_key_value(test_state, phase, "time-sum", 
                  "%g (%s for %d updates)",
                  time_as_double(&stats->time_delta),
                  time_as_string(&stats->time_delta),
                  divisor);

  print_key_value(test_state, phase, "time-ave",
                  "%g (ave over %d updates)",
                  time_as_double(&stats->time_delta) / ((double)divisor),
                  divisor);

  /*test_stats_print(stats, prefix, divisor);*/
  print_basemm_stats(test_state, phase);
  print_analytic_stats(test_state, phase);
  print_desc_stats(test_state, phase);
}

static void
phase_verifier(test_state_t* test_state) {
  const char* phase = "verifier";
  test_stats_t* stats = &test_state->stats_verif;
  long divisor;
  
  /* -- Print input. */
  print_input(test_state);
  
  fprintf(stderr, "%s: beginning verifier run.\n",  __FUNCTION__);
  time_save(&stats->time_begin);

  /* -- Run the application's verifier.
     -- this is a non-tracing, non-reusing (i.e., static) version. */
  divisor = verifier(test_state);
  
  time_save(&stats->time_end);
  time_diff(&stats->time_delta, &stats->time_end, &stats->time_begin);
  fprintf(stderr, "%s: verifier run complete.\n",  __FUNCTION__);
  
  print_key_value(test_state, phase, "time-sum",
                  "%g (%s for %d runs)",
                  time_as_double(&stats->time_delta),
                  time_as_string(&stats->time_delta),
                  divisor);

  print_key_value(test_state, phase, "time-ave",
                  "%g (ave over %d runs)",
                  time_as_double(&stats->time_delta) / ((double)divisor),
                  divisor);
  
  
  /* -- Print output. */
  print_output(test_state, OUTPUT_OF_VERF);
  
  print_basemm_stats(test_state, phase);
}

static void
phase_checkresult(test_state_t* test_state) {
  const char* phase = "verifier";
  int result_check;
  
  fprintf(stderr, "%s: checking verifier result.\n",  __FUNCTION__);
  result_check = cealtesthook_output_check();
  fprintf(stderr, "%s: check complete.\n",  __FUNCTION__);

  if(! result_check) {
    test_state->exit_status = EXIT_FAILURE;
  }
  
  print_key_value(test_state, phase, "check", "%d (%s)",
                  result_check,
                  result_check ? "passed" : "failed");
}

/* -- This is the main application-testing routine.  */
static void test_app(test_state_t* test_state) {

  if( test_flagval( test_state->params.testpower.is_selected ) ) {
    /* Do the power test. */
    testpower_main( test_state );
    
  }
  else {
    const char* phase = NULL; 

    fprintf(stderr, "%s: beginning to generate input.\n",  __FUNCTION__);
    cealtesthook_input_generate(test_state->params.input_size);
    
    print_input(test_state);
    
    print_key_value(test_state, "", "input-size", "%llu (%s)",
                    (unsigned long long) test_state->params.input_size,
                    string_of_size(10, test_state->params.input_size));

    /* Do the test phases that were requested. */
    for(phase = test_state->params.phases;
        phase[0] != 0;
        phase ++) {
      if(!strncmp(PHASE_FROMSCRATCH, phase, 1)) {
        logg(MINUS_SEP_20 "From-Scratch ("PHASE_FROMSCRATCH") " MINUS_SEP_20);
        phase_fromscratch(test_state);
        logg(PLUS_SEP_20 "From-Scratch ("PHASE_FROMSCRATCH") " PLUS_SEP_20);
      }
      else if(!strncmp(PHASE_ALLINSREM, phase, 1)) {
        logg(MINUS_SEP_20 "All Ins/Rem Phase ("PHASE_ALLINSREM") " MINUS_SEP_20);
        phase_allinsrem(test_state);
        logg(PLUS_SEP_20 "All Ins/Rem Phase ("PHASE_ALLINSREM") " PLUS_SEP_20);
      }
      else if(!strncmp(PHASE_ALLINSREM_FROMSCRATCH, phase, 1)) {
        logg(MINUS_SEP_20 "All Ins/Rem Phase ("PHASE_ALLINSREM_FROMSCRATCH") " MINUS_SEP_20);
        phase_allinsrem_fromscratch(test_state);
        logg(PLUS_SEP_20 "All Ins/Rem Phase ("PHASE_ALLINSREM_FROMSCRATCH") " PLUS_SEP_20);
      }
      else if(!strncmp(PHASE_VERIFIER, phase, 1)) {
        logg(MINUS_SEP_20 "Verifier Phase ("PHASE_VERIFIER") " MINUS_SEP_20);
        phase_verifier(test_state);
        logg(PLUS_SEP_20 "Verifier Phase ("PHASE_VERIFIER") " PLUS_SEP_20);
      }
      else if(!strncmp(PHASE_CHECKRESULT, phase, 1)) {
        logg(MINUS_SEP_20 "Check Result Phase ("PHASE_CHECKRESULT")" MINUS_SEP_20);
        phase_checkresult(test_state);
        logg(PLUS_SEP_20 "Check Result Phase ("PHASE_CHECKRESULT")" PLUS_SEP_20);
      }    
      else {
        fprintf(stderr, "don't know what phase this is: %c\n", phase[0]);      
      }
    }
  }
}

test_state_t test_state;

void time_to_die_dammit(int signal) {
  logg(MINUS_SEP_20 "The timer has elapsed.  Goodbye!" MINUS_SEP_20);

  test_state.exit_status = EXIT_FAILURE;
 
  print_key_value( & test_state, "end", "exit_status",
                  "%d (ttl alarm rang after %d sec)", test_state.exit_status,
                   test_state.params.total_ttl );

  exit( test_state.exit_status );
}

int cealtestperf_main(int argc, char** argv) {
  test_state.exit_status = EXIT_SUCCESS;
  
  { /* Setup the signal handler for the alarm. */
    struct sigaction act;
    
    act.sa_handler = time_to_die_dammit;
    act.sa_flags   = 0;
    
    sigemptyset ( & act.sa_mask );
    
    if( sigaction( SIGALRM, &act, NULL ) ) {
      perror("sigaction");
      exit(-1);
    }
  }
  
  test_params_default( &test_state.params );
  test_params_from_argv( &test_state.params, argc, argv );
  
  /* -- Echo the parameters back to the user. */
  test_params_print( &test_state.params, stderr );
  
  /* -- Standard test. */
  test_app( &test_state );

  print_key_value( & test_state, "end", "exit_status",
                   "%d (%s)", test_state.exit_status,
                   ( test_state.exit_status == EXIT_SUCCESS
                     ? "EXIT_SUCCESS"
                     : " != EXIT_SUCCESS" ) );
  
  return test_state.exit_status;
}
