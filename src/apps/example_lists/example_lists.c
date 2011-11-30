/* Matthew Hammer <hammer@mpi-sws.org> */

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

list_t* changed_ptr;
list_t  changed_ptrs_old_value;

void do_insertion() {
  /* pick the pointer to change. */
  changed_ptr = & in->tl->tl->tl->tl;

  /* save where it's pointing to. */
  changed_ptrs_old_value = *changed_ptr;

  /* perform the insertion. */
  *changed_ptr = list_cons_(42, changed_ptrs_old_value);
}

void do_removal() {
  /* grab the inserted cons. */
  cons_t* cons = *changed_ptr;

  /* revert the change. */
  *changed_ptr = changed_ptrs_old_value;

  /* reclaim space of the inserted cons cell. */
  kill(cons);
}

int main() {
  
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
