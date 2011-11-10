/* Matthew Hammer <hammer@tti-c.org> */
/* CEAL testing/profiling system */

#ifndef __CEAL_TEST_TYPES_H__
#define __CEAL_TEST_TYPES_H__

#include <stdio.h>
#include <time.h>
#include <stdint.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>

typedef struct test_params_s       test_params_t;
typedef struct test_stats_s        test_stats_t;
typedef struct test_input_hooks_s  test_input_hooks_t;
typedef struct test_output_hooks_s test_output_hooks_t;
typedef struct test_app_hooks_s    test_app_hooks_t;
typedef struct test_app_s          test_app_t;
typedef struct test_state_s        test_state_t;

#define PHASE_FROMSCRATCH             "i"
#define PHASE_ALLINSREM               "p"
#define PHASE_ALLINSREM_FROMSCRATCH   "P"
#define PHASE_VERIFIER                "v"
#define PHASE_CHECKRESULT             "V"

typedef struct param_spec_s {
  const char* param_flag;
  const char* param_arg_type;
  const char* param_arg_default;
  const char* param_doc_string;
} param_spec_t;

typedef enum {
  TEST_FLAGVAL__FIRST,
  TEST_FLAGVAL_OFF,
  TEST_FLAGVAL_ON,
  TEST_FLAGVAL_FORCE_OFF,
  TEST_FLAGVAL_FORCE_ON,
  TEST_FLAGVAL__LAST,
} test_flagval_t;


/* -- Test Params -- */
struct test_params_s {
  const char*    prefix;
  const char*    phases;
  int            rand_seed;
  int            input_size;
  int            change_size;
  double         change_fraction;
  test_flagval_t loop_verf;
  test_flagval_t tvsigs;
  test_flagval_t print_desc_stats;
  test_flagval_t print_inout;
  test_flagval_t inout_files;
  test_flagval_t verf_all_updates;
  long           total_ttl;  
  
  /* rlim_max_as: resource limit for maximum virtual memory (address space)
     See: RLIMIT_AS from manpage getrlimit(2) */
  rlim_t rlim_max_as;

  /* rlim_stack: resource limit for stack size (in bytes). */
  rlim_t rlim_stack;

  /* Parameters that are specifically used for testing power
     consumption. */
  struct testpower_s {
    test_flagval_t is_selected;
    const char*    experiment;
    long           runtime;
    int            initsleep;
  } testpower;  
};

typedef struct timeval timeval_t;

/* -- Test Stats -- */
struct test_stats_s {
  timeval_t      time_begin;
  timeval_t      time_end;
  timeval_t      time_delta;
};

/* -- Test State -- */
struct test_state_s {
  test_params_t  params;
  
  /* stats -- fromscratch */
  test_stats_t   stats_fromscratch;
  
  /* stats -- changeall */
  test_stats_t   stats_changeall;

  /* stats, outputs -- verify */
  test_stats_t   stats_verif;

  /* stats -- testpower */
  test_stats_t  stats_testpower;
  
  /* The exit status
     (e.g., for use in batch regression tests.) */
  int exit_status;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* CEAL Test-Hooks

   The following functions are to be provided by the CEAL program
   being tested (they are included in the code compiled by the CEAL
   compiler).  They are tasked with manipulating the CEAL heap in
   various application-specific ways, as well as running the core
   program being tested.
*/

/* Inputs. */
extern void cealtesthook_input_generate(long size);
extern void cealtesthook_input_print(FILE* file);
extern void cealtesthook_input_iter_begin(long change_size);
extern void cealtesthook_input_iter_next();
extern int  cealtesthook_input_iter_isdone();
extern void cealtesthook_input_iter_change();
extern void cealtesthook_input_iter_revert();

/* Run the app. */
extern void cealtesthook_run_core();
extern void cealtesthook_run_verf();

/* Outputs. */

typedef enum cealtesthook_output_version_e {
  OUTPUT_OF_CORE = 0,
  OUTPUT_OF_VERF = 1
} cealtesthook_output_version_t ;

extern void cealtesthook_output_print(FILE* file, cealtesthook_output_version_t v);
extern int  cealtesthook_output_check();

/* @} */

#endif
