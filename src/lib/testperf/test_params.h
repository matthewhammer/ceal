#ifndef __TEST_PARAMS_H__
#define __TEST_PARAMS_H__

#include <stdio.h>
#include "test_types.h"

void        test_params_print(test_params_t* test_params, FILE* f);
void        test_params_default(test_params_t* test_params);
int         test_params_from_argv(test_params_t* test_params, int argc, char** argv);

const char* test_flagval_name(test_flagval_t flagval);
int         test_flagval(test_flagval_t flagval);

void
testpower_main( test_state_t* test_state );

/* List all applications for which there are tests available.
   Provided by 'apptest' code (i.e., the testing code that is specific
   to application being tested). */
extern void
test_app_listall(FILE* f,
                 const char* prefix,
                 const char* suffix,
                 const char* end);


#endif
