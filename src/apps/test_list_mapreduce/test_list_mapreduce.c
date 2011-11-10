/* Matthew Hammer <hammer@mpi-sws.org>
   Reinhard Murz <munz@mpi-sws.org>
*/
      
#include "main.c"

#define Mapf     mapf
#define Comparef comparef
#define Reducef  reducef
#define K1_t     long
#define V1_t     long
#define K2_t     long
#define V2_t     long
#define K3_t     long
#define V3_t     long

#include "list_mapreduce_decls_functor.c"

#define Ng(name) my_##name

#include "list_mapreduce_functor.c"


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* List-reduction for lists of type V2_t (long). */

V2_t V2_sum(void* dummy, V2_t a, V2_t b) {
  return a + b;
}

#define List_t       V2_list_t
#define List_tl_t    V2_list_tl_t
#define List_hd_t    V2_list_hd_t
#define List_cons    V2_list_cons
#define List_hd      V2_list_hd
#define List_tl      V2_list_tl
#define Monoid_binop V2_sum
#define Reduce_fun   V2_list_sum
#include "list_reduce_functor.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Convenience functions. */
/* Each creates a cons cell that holds a key/value pair. */

KV1_list_t KV1_list_cons_kv(K1_t key, V1_t val) {
  memo;
  KV1_t* kv = alloc(struct KV1_s);
  kv->key = key;
  kv->val = val;
  KV1_list_t l = KV1_list_cons(kv);
  return l;
}

KV2_list_t KV2_list_cons_kv(K2_t key, V2_t val) {
  KV2_list_t result;
  memo {
    KV2_t* kv = alloc(struct KV2_s);
    kv->key = key;
    kv->val = val;
    result = KV2_list_cons(kv);
  }
  return result;
}

KV3_list_t KV3_list_cons_kv(K3_t key, V3_t val) {
  memo;
  KV3_t* kv = alloc(struct KV3_s);
  kv->key = key;
  kv->val = val;
  KV3_list_t l = KV3_list_cons(kv);
  return l;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* We provide the following to the map-reduce framework:
   mapf, comparef and reducef . */

/* Output list should be appended to out; its tail should be returned. */
KV2_list_t* mapf (K1_t key, V1_t val, KV2_list_t* out) {

  if ( key % 2 ) {
    KV2_list_t c1 = KV2_list_cons_kv(key, val * 1);
    KV2_list_t c2 = KV2_list_cons_kv(key, val * 2);
    KV2_list_t c3 = KV2_list_cons_kv(key, val * 3);
    
    *out = c1;
    *KV2_list_tl(c1) = c2;
    *KV2_list_tl(c2) = c3;
  
    return KV2_list_tl(c3);
  }
  else {
    /* Test the case where no output is produced: */
    return out;
  }
}

/* Comparison used to sort by key, and to compare keys for equality. */
long comparef (K2_t k2a, K2_t k2b) {
  if ( k2a < k2b ) { return 1; }
  else { return 0; }
}

/* Output list should be appended to out; its tail should be returned. */
KV3_list_t* reducef (K2_t key, V2_list_t* vals,  KV3_list_t* out) {

  if ( key % 3 ) { 
    V2_t sum = memo(V2_list_sum(vals, NULL));
    
    KV3_list_t c = KV3_list_cons_kv( key, sum );
    
    *out = c;
    return KV3_list_tl(c);
  }
  else {
    /* Test the case where no output is produced: */
    return out;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

KV1_list_tl_t  input;
KV1_list_tl_t *input_iter;

KV3_list_t  core_out;
KV3_list_t  verf_out;

void my_core() {
  my_mapreduce(&input, &core_out);
}

void cealtesthook_run_core() {
  core(my_core)();
}

void cealtesthook_run_verf() {
  my_mapreduce(&input, &verf_out);
}

void cealtesthook_input_generate(long size) {
  KV1_list_t* d = &input;  

  for(long i = 0; i < size; i++) {
    KV1_list_t c = KV1_list_cons_kv(i, i);
    *d = c;
    d = KV1_list_tl(c);
  }
  
  *d = NULL;
  return;
}

void cealtesthook_input_print(FILE* file) {
  KV1_list_t l = input;
  fprintf(file, "[");
  while(l != NULL) {
    KV1_list_t next = *KV1_list_tl(l);
    KV1_t* kv = KV1_list_hd(l);

    /* Print the key/value pair. */
    fprintf(file, "(%ld,%ld)%s", kv->key, kv->val,
            (next ? ", " : "") );
    
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
    fprintf(file, "(%ld,%ld)%s", kv->key, kv->val,
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
