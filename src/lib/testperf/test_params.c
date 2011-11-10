#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>

#include "test_params.h"
#include "test_time.h"

extern void ceal_tvsig_stop();

const char* test_flagval_name(test_flagval_t flagval) {
  static const char* flagval_names[] = {
    "(first)", "off", "on", "force-off", "force-on", "(last)"
  };
  assert(flagval > TEST_FLAGVAL__FIRST);
  assert(flagval < TEST_FLAGVAL__LAST);
  return flagval_names[flagval];
}

int test_flagval(test_flagval_t flagval) {
  return (flagval == TEST_FLAGVAL_ON ||
          flagval == TEST_FLAGVAL_FORCE_ON);
}

void test_params_print(test_params_t* test_params, FILE* f) {
  const char* fmt_int = "test-params: %20s = %d\n";
  const char* fmt_lng = "test-params: %20s = %ld\n";
  const char* fmt_flg = "test-params: %20s = %s\n";
  const char* fmt_dbl = "test-params: %20s = %0.2f\n";
  /*const char* fmt_str = "test-params: %15s = %s\n";*/

  fprintf(f, fmt_flg, "prefix",            test_params->prefix);
  fprintf(f, fmt_flg, "phases",            test_params->phases);
  fprintf(f, fmt_int, "rand_seed",         test_params->rand_seed);
  fprintf(f, fmt_lng, "rlim-max-as",       test_params->rlim_max_as);
  fprintf(f, fmt_lng, "rlim-stack",        test_params->rlim_max_as);
  fprintf(f, fmt_int, "input-size",        test_params->input_size);
  fprintf(f, fmt_int, "change-size",       test_params->change_size);
  fprintf(f, fmt_dbl, "change-fraction",   test_params->change_fraction);
  fprintf(f, fmt_flg, "tvsigs",            test_flagval_name(test_params->tvsigs));
  fprintf(f, fmt_flg, "loop-verf",         test_flagval_name(test_params->print_inout));
  fprintf(f, fmt_flg, "verf-all-updates",  test_flagval_name(test_params->verf_all_updates));
  fprintf(f, fmt_flg, "print-desc-stats",  test_flagval_name(test_params->print_desc_stats));
  fprintf(f, fmt_flg, "print-inout",       test_flagval_name(test_params->print_inout));
  fprintf(f, fmt_flg, "inout-files",       test_flagval_name(test_params->inout_files));

  fprintf(f, fmt_flg, "testpower",            test_flagval_name(test_params->testpower.is_selected));
  fprintf(f, fmt_flg, "testpower-experiment", test_params->testpower.experiment);
  fprintf(f, fmt_int, "testpower-runtime",    test_params->testpower.runtime);
  fprintf(f, fmt_int, "testpower-initsleep",  test_params->testpower.initsleep);
}



void test_params_default(test_params_t* test_params) {
  static const char* NONE = "";
  
  test_params->prefix            = NONE;
  test_params->phases            = (PHASE_FROMSCRATCH
                                    PHASE_ALLINSREM
                                    PHASE_VERIFIER
                                    PHASE_CHECKRESULT);
  test_params->rand_seed         = 0;
  test_params->input_size        = 10;
  test_params->change_size       = 1;
  test_params->change_fraction   = 1.0;
  test_params->loop_verf         = TEST_FLAGVAL_ON;
  test_params->tvsigs            = TEST_FLAGVAL_ON;
  test_params->verf_all_updates  = TEST_FLAGVAL_OFF;
  test_params->print_inout       = TEST_FLAGVAL_OFF;
  test_params->print_desc_stats  = TEST_FLAGVAL_OFF;
  test_params->inout_files       = TEST_FLAGVAL_OFF;
  test_params->rlim_max_as       = RLIM_INFINITY;
  test_params->rlim_stack        = RLIM_INFINITY;
  test_params->total_ttl         = 0; /* << 0 is the magic number to not enable the signal (see signal.h). */

  test_params->testpower.is_selected = TEST_FLAGVAL_OFF;
  test_params->testpower.experiment  = "";
  test_params->testpower.runtime     = 30000000;
  test_params->testpower.initsleep   = 10;
  
  { /* Default: no limit on the stack: */
    struct rlimit setrlimit_arg = {RLIM_INFINITY, RLIM_INFINITY};
    int retval = setrlimit(RLIMIT_STACK, &setrlimit_arg);
    if(retval != 0) {
      perror("setrlimit");
      exit(EXIT_FAILURE);
    }
  }

  /* Default: seed with 0 */
  srand(0);
  srand48(0);

#if RAND_MAX < INT_MAX
#error RAND_MAX is too small --- things may go badly.
#endif
}

int test_params__get_argv_int(char** argv, int argc, int argi) {
  if(argi >= argc) {
    fprintf(stderr, "expected an integer after %s\n", argv[argi-1]);
    exit(-1);
  }    
  return atoi(argv[argi]);
}

double test_params__get_argv_double(char** argv, int argc, int argi) {
  if(argi >= argc) {
    fprintf(stderr, "expected a double after %s\n", argv[argi-1]);
    exit(-1);
  }    
  return strtod(argv[argi], NULL);
}

char* test_params__get_argv_string(char** argv, int argc, int argi) {
  if(argi >= argc) {
    fprintf(stderr, "expected a string after %s\n", argv[argi-1]);
    exit(-1);
  }    
  return argv[argi];
}

#define NOT_APPL "---"

param_spec_t param_specs[] = {
  {"Flag", "Arg Type", "Default", "Description"},
  
  {"-h, --help", NOT_APPL, NOT_APPL,
   "Prints this help information"},
  
  {"-input-size", "int", "10",
   "The total input size"},

  {"-change-size", "int", "1",
   "Also known as 'batch changes'; sets the size of multiple-element input changes."
   "  Single-element changes are the default."},
  
  {"-change-fraction", "double", "1.0",
   "The fraction of input elements to change during change-propagation"},
  
  {"-phases", "[ipPvV]*", "ipvV",
   "i = initial-run, p = changes-via-propagation, "
   "P = changes-via-fromscratch, v = verifier, V = verifier-output-check"},

  {"-testpower", "(verf|cp)", NOT_APPL,
   "verf = verifier loop; cp = change-propagation loop; (See also: -runtime)."},

  {"-runtime", "int", "30000000",
   "for use with -testpower; sets the measurement duration of the experiment, in microsec"},

  {"-initsleep", "int", "10",
   "for use with -testpower; before running experiment, sleeps for the given time, in seconds"},

  {"-verf-all-updates", NOT_APPL, "off",
   "runs the verifier and verifier-output-check phases after each change"},
  
  {"+loop-verf", NOT_APPL, "off",
   "disables looping the verifier; by default, we loop it for accurate timing measurements"},

  {"+tvsig", NOT_APPL, "off",
   "CEAL-TV signals: After a CEAL compilation flag (--ceal-tv-signals) enables them,"
   " this flag later disables them. When not enabled, this is a no-op."},
  
  {"-print-inout", NOT_APPL, "off",
   "prints the input and output after each phase"},
  
  {"-print-desc-stats", NOT_APPL, "off",
   "prints statistics for each trace node descriptor; helpful for profiling"},
  
  {"-inout-files", NOT_APPL, "off",
   "like -print-inout, but uses output files rather than stdout"},
  
  {"-prefix", "string", "\"\"",
   "prepends the given prefix to the data paths produced on stdout"},
  
  {"-srand", "int", NOT_APPL,
   "seeds the random number generator by either given int; or for \"time\", the system clock"},
  
  {"-rlim-stack", "int", NOT_APPL,
   "uses setrlimit(2) to limit the stack size; in megabytes"},
  
  {"-rlim-max-as", "int", NOT_APPL,
   "uses setrlimit(2) to limit the process's address space; in megabytes"},
  
  {"-ttl", "int", NOT_APPL,
   "sets the time to live, in seconds; after this, the process will kill itself"}
};

int test_params_from_argv(test_params_t* test_params,
                          int argc, char** argv) {
  int i;
  for(i = 1; i < argc; i++) {
    char* arg = argv[i];
    if(!strncmp("-actk", arg, 5) ||
       !strncmp("+actk", arg, 5)) {
      return i;
    }
    else if(!strcmp("-h", arg) ||
            !strcmp("--help", arg)) {
      int i;
      for(i = 0; i < sizeof(param_specs) / sizeof(param_spec_t); i++) {
        printf("\t%-20s | %-10s | %-10s %s.\n",
               param_specs[i].param_flag,
               param_specs[i].param_arg_type,
               param_specs[i].param_arg_default,
               param_specs[i].param_doc_string);
        if(i > 0) {
          printf("\t%-20s | %-10s | %-10s %s\n", "", "", "", "");
        }
        else {
          const char* hr = "-----------------";
          printf("\t%s%s%s%s%s%s\n", hr, hr, hr, hr, hr, hr);
        }
      }      
      exit(0);
    }    
    else if(!strcmp("-input-size", arg)) {
      test_params->input_size =
        test_params__get_argv_int(argv, argc, ++i);
    }
    else if(!strcmp("-change-size", arg)) {
      test_params->change_size =
        test_params__get_argv_int(argv, argc, ++i);
      assert( test_params->change_size >= 1 );
    }
    else if(!strcmp("-change-fraction", arg)) {
      test_params->change_fraction =
        test_params__get_argv_double(argv, argc, ++i);
    }
    else if(!strcmp("-phases", arg)) {
      char* phases = test_params__get_argv_string(argv, argc, ++i);
      test_params->phases = phases;
    }
    else if(!strcmp("-testpower", arg)) {
      test_params->testpower.experiment  = test_params__get_argv_string(argv, argc, ++i);
      test_params->testpower.is_selected = TEST_FLAGVAL_FORCE_ON;
    }
    else if(!strcmp("-runtime", arg)) {
      test_params->testpower.runtime = test_params__get_argv_int(argv, argc, ++i);
    }
    else if(!strcmp("-initsleep", arg)) {
      test_params->testpower.initsleep = test_params__get_argv_int(argv, argc, ++i);
    }    
    else if(!strcmp("-verf-all-updates", arg)) {
      test_params->verf_all_updates = TEST_FLAGVAL_FORCE_ON;
    }
    else if(!strcmp("+tvsigs", arg)) {
      test_params->tvsigs = TEST_FLAGVAL_FORCE_OFF;
      ceal_tvsig_stop();
    }    
    else if(!strcmp("+loop-verf", arg)) {
      test_params->loop_verf = TEST_FLAGVAL_FORCE_OFF;
    }    
    else if(!strcmp("-print-inout", arg)) {
      test_params->print_inout = TEST_FLAGVAL_FORCE_ON;
    }
    else if(!strcmp("-print-desc-stats", arg)) {
      test_params->print_desc_stats = TEST_FLAGVAL_FORCE_ON;
    }
    else if(!strcmp("+print-inout", arg)) {
      test_params->print_inout = TEST_FLAGVAL_FORCE_OFF;
    }
    else if(!strcmp("-inout-files", arg)) {
      test_params->print_inout = TEST_FLAGVAL_ON;
      test_params->inout_files = TEST_FLAGVAL_FORCE_ON;
    }
    else if(!strcmp("+inout-files", arg)) {
      test_params->inout_files = TEST_FLAGVAL_FORCE_OFF;
    }
    else if(!strcmp("-prefix", arg)) {
      test_params->prefix = test_params__get_argv_string(argv, argc, ++i);
    }    
    else if(!strcmp("-srand", arg)) {
      int seed;
      char* srand_val;

      /* First try to get the random seed as a string, see if its a
         special value (e.g., "time") */        
      srand_val = test_params__get_argv_string(argv, argc, ++i);
      
      if(!strcmp(srand_val, "time") ||
         !strcmp(srand_val, "clock")) {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        seed = tv.tv_usec;        
      }
      else {
        /* Instead of seeding by clock, use the specified seed (an int) */
        seed = test_params__get_argv_int(argv, argc, i);
      }
      srand(seed);
      srand48(seed);
      test_params->rand_seed = seed;
    }
    else if(!strcmp("-rlim-stack", arg)) {
      rlim_t megabytes = test_params__get_argv_int(argv, argc, ++i);
      rlim_t bytes = megabytes << 20;
      struct rlimit setrlimit_arg = {bytes, bytes};
      int retval = setrlimit(RLIMIT_STACK, &setrlimit_arg);
      test_params->rlim_stack = bytes;
      if(retval != 0) {
        perror("setrlimit");
        exit(EXIT_FAILURE);
      }
    }    
    else if(!strcmp("-rlim-max-as", arg)) {
      rlim_t megabytes = test_params__get_argv_int(argv, argc, ++i);
      rlim_t bytes = megabytes << 20;
      struct rlimit setrlimit_arg = {bytes, bytes};
      int retval = setrlimit(RLIMIT_AS, &setrlimit_arg);
      test_params->rlim_max_as = bytes;
      if(retval != 0) {
        perror("setrlimit");
        exit(EXIT_FAILURE);
      }
    }
    else if(!strcmp("-ttl", arg)) {
      test_params->total_ttl = test_params__get_argv_int(argv, argc, ++i); 
      alarm( test_params->total_ttl );
    }
    else {
      fprintf(stderr, __FILE__": %s: argument not recognized: %s\n",__FUNCTION__,arg);
      exit(EXIT_FAILURE);
    }
  }
  return i;
}

