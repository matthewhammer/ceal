/* Matthew Hammer <hammer@mpi-sws.org>
   Reinhard Murz <munz@mpi-sws.org>
*/
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "main.c"

#define Mapf      mapf
#define Equalsf   equalsf
#define Lessthanf lessthanf
#define Reducef   reducef
#define K1_t      long  foreign_c
#define V1_t      char* foreign_c
#define K2_t      char* foreign_c
#define V2_t      long  foreign_c
#define K3_t      char* foreign_c
#define V3_t      long  foreign_c

#include "list_mapreduce_decls_functor.c"
#define Ng(name) my_##name
#include "list_mapreduce_functor.c"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Convenience functions. */
/* Each creates a cons cell that holds a key/value pair. */

KV1_list_t KV1_list_cons_kv(K1_t key, V1_t val) {
  memo;
  KV1_t* kv = alloc(struct KV1_s);
  kv->key = key;
  kv->val = val;
  return KV1_list_cons(kv);
}

KV2_list_t KV2_list_cons_kv(K2_t key, V2_t val) {
  memo;
  KV2_t* kv = alloc(struct KV2_s);
  kv->key = key;
  kv->val = val;
  return KV2_list_cons(kv);
}

KV3_list_t KV3_list_cons_kv(K3_t key, V3_t val) {
  memo;
  KV3_t* kv = alloc(struct KV3_s);
  kv->key = key;
  kv->val = val;
  return KV3_list_cons(kv);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


static long is_null_str(char* c) foreign_c {
  return c[0] == '\0';
}

static void set_null_char(char* c) foreign_c {
  c[0] = '\0';
}

typedef struct {
  char* foreign_c copy;
} strsep_ret_val_t;

static char* strsep_wrapper(char* str, const char* delim, strsep_ret_val_t* ret) foreign_c {
  assert(str);
  assert(delim);
  assert(ret);
  char* copy;
  int i;
  int j;
  int first_delim; /* = strlen(str);*/
  int no_more_tokens = 0;
  for (i=0; str[i] != '\0'/*i<strlen(str)*/; i++) {
    for (j=0; delim[j] != '\0'/*j<strlen(delim)*/; j++) {
      if (str[i] == delim[j] ) {
        first_delim = i;
        goto done;
      }
    }
  }
  no_more_tokens = 1;
  first_delim = i;
 done:  
  copy = malloc(sizeof(char) * (first_delim + 1));
  strncpy(copy, str, first_delim);
  copy[first_delim] = '\0';
  ret->copy = copy;
  if ( no_more_tokens ) {
    return NULL;
  }
  return &str[first_delim + 1];
}

/* Output list should be appended to out; its tail should be returned. */
KV2_list_t* mapf (K1_t key, V1_t val,  KV2_list_t* out) {
  const char *delim = " ,.\n\t:;'\"?!()[]{}/-<>=_";
  char *in = val;
  
  while ( in != NULL ) 
  {
    /* BUG-FIX (Aug 9 2011): We were calling strsep directly from this
       function, and we were giving it the address of a local
       variable, which it overwrote.  Allowing ordinary C code to
       write our memory generally causes corruption of our runtime
       data structures.  The solution is to use a wrapper function
       (strsep_wrapper) above.
     */
    strsep_ret_val_t* ret = alloc(strsep_ret_val_t);
    /*foreign_producer(ret);*/
    in = strsep_wrapper( in, delim , ret );
    
    if ( ! is_null_str( ret->copy ) ) {
      /*printf("copy=%s\n", ret->copy);*/
      
      /*
        KV2_t* kv = alloc(KV2_t);
        kv->key = old_in;
        kv->val = 1;
        KV2_list_t c = alloc(struct KV2_cons_s);
          c->hd = kv;
      */
      KV2_list_t c = KV2_list_cons_kv( ret->copy , 1);
      *out = c;
      out = KV2_list_tl(c);
    }
  }
  
  return out;
}

/* Comparison used to sort by key, and to compare keys for equality. */
long equalsf (K2_t kv1, K2_t kv2) {
  return (strcmp((const char*) kv1, (const char*) kv2) == 0);
}

/* Comparison used to sort by key, and to compare keys for equality. */
long lessthanf (K2_t kv1, K2_t kv2) {
  return (strcmp((const char*) kv1, (const char*) kv2) == -1);
}

long sum(void* dummy, long a, long b) {
    return a+b;
}

long comp_counts (KV3_t* a, KV3_t* b) {
  return (a->val > b->val);
  /*
    if (a->val > b->val) {
        return -1;
    } else if (a->val < b->val) {
        return 1;
    } else {
        return 0;
    }
  */
}

#define List_t     V2_list_t
#define List_tl_t  V2_list_tl_t
#define List_hd_t  V2_list_hd_t
#define List_cons  V2_list_cons
#define List_hd    V2_list_hd
#define List_tl    V2_list_tl
#define Monoid_binop sum
#define Reduce_fun list_sum
#include "list_reduce_functor.h"
#undef List_t
#undef List_tl_t
#undef List_hd_t
#undef List_cons
#undef List_hd
#undef List_tl
#undef Monoid_binop
#undef Reduce_fun

#define List_t     KV3_list_t
#define List_tl_t  KV3_list_tl_t
#define List_hd_t  KV3_list_hd_t
#define List_cons  KV3_list_cons
#define List_hd    KV3_list_hd
#define List_tl    KV3_list_tl
#define Hd_less_than comp_counts
#define Ng(name) my_sort_##name
#include "list_mergesort_functor.c"
#undef List_t
#undef List_tl_t
#undef List_hd_t
#undef List_cons
#undef List_hdes
#undef List_tl
#undef Hd_less_than

/* Output list should be appended to out; its tail should be returned. */
KV3_list_t* reducef (K2_t key, V2_list_t* vals,  KV3_list_t* out) {
#if 1
  
  KV3_list_t c = KV3_list_cons_kv(key, list_sum(vals, NULL));
  KV3_t* kv = alloc(KV3_t);
  *out = c;
  return KV3_list_tl(c);

#else
  V2_t sum;
  /* TODO :
     -- compute sum from vals with a single list traversal
     -- create a KV3 cons cell (like above) with the sum.
  */  
#endif
}




/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

KV1_list_tl_t  input;
KV1_list_tl_t *input_iter;

KV3_list_t  core_out;
KV3_list_t  verf_out;

void run(KV3_list_t* out) {
  KV3_list_t  out_unsorted;
#if 0
  my_mapreduce(&input, &out_unsorted);
  my_sort_mergesort(&out_unsorted, out);
#else
  my_mapreduce(&input, &out);
#endif
}

void cealtesthook_run_core() {
  core(run)(&core_out);
}

void cealtesthook_run_verf() {
  run(&verf_out);
}

#define BUFFSIZE (1<<20)

void cealtesthook_input_generate(long size) {
  KV1_list_t* list_tail = &input;
  const char* inputFilename = "/var/tmp/ceal_mapreduce_input.xml";
  /*const char* inputFilename = "/PLS/bigdata/work/ceal/wiki/small-sample.xml";*/
  char* buffer = (char*) malloc(BUFFSIZE + 1);
  assert(buffer);
  fprintf(stderr, "Buffer pointer buffer points to: %lu\n", buffer);

  int fd = open(inputFilename, O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "Can't open input file %s!\n", inputFilename);
    exit(1);
  }

  long left_size = BUFFSIZE;
  long curr_read_bytes = 1;
  long n_read_bytes = 0;
  char* insert_here = buffer;

  while (!(left_size==0 || curr_read_bytes==0)) {
      fprintf(stderr, "Buffer pointer insert_here points to: %lu\n", insert_here);
      curr_read_bytes = read(fd, insert_here, left_size);
      fprintf(stderr, "curr_read_bytes has value: %ld\n", curr_read_bytes);
      if (curr_read_bytes<0) {
          fprintf(stderr, "Error reading from file: %s\n", strerror( errno ));
      }
      assert(curr_read_bytes>=0);
      n_read_bytes += curr_read_bytes;
      left_size -= curr_read_bytes;
      insert_here = &insert_here[curr_read_bytes];
  }

  fprintf(stderr, "read %ld bytes\n", n_read_bytes);

  assert(n_read_bytes > 0);
  assert(n_read_bytes <= BUFFSIZE);
  set_null_char( &(buffer[n_read_bytes]) );

  /* 1 Chunk = 1 "Page" */
  long page_idx = 0;
  char* buff_p = buffer;

  while( (page_idx < size) &&
	 buff_p < &(buffer[n_read_bytes]) ) {

    /*fprintf(stderr, "reading page %ld\n", page_idx);*/

    char* page_start;
    char* page_end;

    page_start = strstr(buff_p,"<page");
    /*fprintf(stderr, "page_start=%p\n", page_start);*/

    if( page_start ){
      page_end = strstr(page_start, "</page");
      /*fprintf(stderr, "page_end=%p\n", page_end);*/
    }
    else {
      fprintf(stderr, "couldn't find page start\n");
      break;
    }

    if ( page_end && page_start ){
      /*fprintf(stderr, "appending page %d\n", page_idx);*/
      KV1_list_t c = KV1_list_cons_kv(page_idx, page_start);
      set_null_char( page_end );
      *list_tail = c;
      list_tail = KV1_list_tl(c);
    }
    else {
      fprintf(stderr, "couldn't find page end\n");
      break;
    }

    buff_p = page_end + 1;
    page_idx ++;
  }
  fprintf(stderr, "done reading\n");
  /* BUG-FIX (Aug 8 2011):
     -- We weren't properly setting the tail to NULL. That is:

     list_tail = NULL;

     -- Should have been:

     *list_tail = NULL;
  */
  *list_tail = NULL;
}

void cealtesthook_input_print(FILE* file) {
  KV1_list_t l = input;
  fprintf(file, "[");
  while(l != NULL) {
    KV1_list_t next = *KV1_list_tl(l);
    KV1_t* kv = KV1_list_hd(l);

    /* Print the key/value pair. */
    /*
    fprintf(file, "(%ld,\"%s\")%s", kv->key, kv->val,
	    (next ? ", " : "") );
    */
    l = next;
  }
  fprintf(file, "]");
  return;
}

void cealtesthook_input_iter_begin() {
  input_iter = &input;
}

void cealtesthook_input_iter_next() {
  input_iter = KV1_list_tl(*input_iter);
}

int cealtesthook_input_iter_isdone() {
  return (*input_iter == NULL);
}

KV1_list_tl_t removed_cell = NULL;

void cealtesthook_input_iter_change() {
  removed_cell = *input_iter;
  *input_iter  = *KV1_list_tl(removed_cell);
}

void cealtesthook_input_iter_revert() {
  *input_iter = removed_cell;
}

void cealtesthook_output_print(FILE* file, int core_or_verf) {
  KV3_list_t l;

  if( core_or_verf == 0 ) { l = core_out; }
  else                    { l = verf_out; }

  fprintf(file, "[");

  while(l != NULL) {
    KV3_list_t next = *KV3_list_tl(l);
    KV3_t*     kv   =  KV3_list_hd(l);

    /* Print the key/value pair. */
    fprintf(file, "(%s,%ld)%s", kv->key, kv->val,
            (next ? ", " : "") );

    l = next;
  }

  fprintf(file, "]");
}

int cealtesthook_output_check() {
  KV3_list_t l1 = core_out;
  KV3_list_t l2 = verf_out;

  while(l1 != NULL && l2 != NULL) {
    KV3_t*     kv1   =  KV3_list_hd(l1);
    KV3_t*     kv2   =  KV3_list_hd(l2);

    if( kv1->key != kv1->key ||
        kv1->val != kv2->val )
      return 0;

    l1 = *KV3_list_tl(l1);
    l2 = *KV3_list_tl(l2);
  }

  if(l1 != NULL || l2 != NULL)
    return 0;

  return 1;
}
