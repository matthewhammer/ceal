/* Matthew Hammer <hammer@mpi-sws.org> */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "cealtesthook_dummies.c"

#define db(FMT,...) printf("-- %s:%d: " FMT "\n", __FUNCTION__,__LINE__,##__VA_ARGS__)

/* Let's have lists that hold longs. */
typedef long data_t;

data_t some_function(data_t d) {
  return labs(d);
}

long some_predicate(data_t d) {
  return labs(d) == d;
}

/* TODO: Change this to include your memoized version of the file. */
#include "example_lists_nomemo.c"

list_t in;
list_t map_out;
list_t filter_out;
list_t split_out1;
list_t split_out2;
list_t reverse_out;

void the_core() {
  list_map(in, &map_out);  
  list_filter(in, &filter_out);  
  list_split(in, &split_out1, &split_out2);  
  list_reverse(in, nil, &reverse_out);
}

void print_input() {  
  list_print("in", in);
}

void print_output() {
  list_print("map_out", map_out);
  list_print("filter_out", filter_out);
  list_print("split_out1", split_out1);
  list_print("split_out1", split_out2);
  list_print("reverse_out", reverse_out);
}

list_t* finger;
list_t  fingers_old_value;

/* pick the pointer to change. */
void setup_finger (long index) { assert( index >= 0 );
  finger = & in;
  while( index > 0 ) { 
    finger = &((*finger)->tl); 
    index--; 
  }
}

void do_insertion() {
  /* save where it's pointing to. */
  fingers_old_value = *finger;

  /* perform the insertion. */
  *finger = list_cons_(42, fingers_old_value);
}

void do_removal() {
  /* grab the inserted cons. */
  cons_t* cons = *finger;

  /* revert the change. */
  *finger = fingers_old_value;

  /* reclaim space of the inserted cons cell. */
  kill(cons);
}

int main(int argc, char** argv) {
  
  in = (list_cons_
        (0, list_cons_
         (+1, list_cons_
          (-2, list_cons_
           (-3, list_cons_
            (+4, list_cons_
             (-5, list_cons_
              (+6, list_cons_
               (-7, nil)
               ) ) ) ) ) ) ) );

  print_input();
  core(the_core)();
  print_output();

  setup_finger(5);
  
  printf("\ndoing an insertion\n");
  do_insertion();
  print_input();
  propagate;
  print_output();

  printf("\ndoing a removal\n");
  do_removal();
  print_input();
  propagate;
  print_output();
  
  return 0;
}
