/* Matthew Hammer <hammer@mpi-sws.org> */

#include "cealtesthook_dummies.c"

typedef struct {
  long one;
  long two;
} data_t;

data_t* data;

void print() {
  printf("%d\n", data->one);
}

void the_core() {
  cut { data->two = data->one; }
  cut { data->one = data->two * 2; }
}

int main(int argc, char** argv) {
  data = alloc(data_t);
  data->one = 1;

  print();
  core(the_core)();
  
  for(int i = 0; i < 10; i++) {
    print();
    propagate;
  }
  print();
  
  return 0;
}
