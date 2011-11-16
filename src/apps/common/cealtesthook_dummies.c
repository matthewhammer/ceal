/* Matthew Hammer <hammer@ttic.edu> */

/* Include this file when you do not want to use the standard CEAL
   test harness.  This file effectively fills in the cealtesthooks
   with dummy functions. */

/* See also: main.c */

/* Inputs. */
void cealtesthook_input_generate(long size) { }
void cealtesthook_input_print(FILE* file) { }
void cealtesthook_input_iter_begin(long change_size) { }
void cealtesthook_input_iter_next() { }
int  cealtesthook_input_iter_isdone() { return 1; }
void cealtesthook_input_iter_change() { }
void cealtesthook_input_iter_revert() { }

/* Run the app. */
void cealtesthook_run_core() { }
void cealtesthook_run_verf() { }

/* Outputs. */
typedef enum cealtesthook_output_version_e {
  OUTPUT_OF_CORE = 0,
  OUTPUT_OF_VERF = 1
} cealtesthook_output_version_t ;

void cealtesthook_output_print(FILE* file, cealtesthook_output_version_t v) { }
int  cealtesthook_output_check() { return 0; }
