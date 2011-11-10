/* Matthew Hammer <hammer@mpi-sws.org>
   Reinhard Murz <munz@mpi-sws.org>
*/

/* Parameters to functor: */
#if  defined(Mapf)                 \
  && defined(Equalsf)              \
  && defined(Lessthanf)            \
  && defined(Reducef)              \
  && defined(Ng) 
#else
#error Undefined functor parameters.
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* We provide the following: */

void Ng(mapreduce)(KV1_list_t* in, KV3_list_t* out);


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long KV2_lessthan(KV2_t* kv1, KV2_t* kv2)
{ return Lessthanf(kv1->key, kv2->key); }

#define List_t       KV2_list_t
#define List_tl_t    KV2_list_tl_t
#define List_hd_t    KV2_list_hd_t
#define List_cons    KV2_list_cons
#define List_hd      KV2_list_hd
#define List_tl      KV2_list_tl
#define Hd_less_than KV2_lessthan
#include "list_mergesort_functor.c"
#undef List_t
#undef List_tl_t
#undef List_hd_t
#undef List_cons
#undef List_hd
#undef List_tl
#undef Hd_less_than

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* Invokes Mapf on each input key/value pair. */
static void Ng(map)(KV1_list_t  in,
                    KV2_list_t* out)
{
  if(in == NULL) {
#if 0
    printf("map: done\n");
#endif
    *out = NULL;
  }
  else {
    KV1_t*        kv = KV1_list_hd(in);
    KV2_list_t* out2;
    KV2_list_t* out3;

    memo {
      out2 = alloc( KV2_list_t );
      out3 = Mapf( kv->key, kv->val, out2);
    }

    if(out2 != out3) {
      /* Case 1: The map function produced some elements. */
#if 0
      printf("map: got output\n");
#endif
      *out = *out2;
    }
    else {
      /* Case 2: The map function produced no elements. */
#if 0
      printf("map: no output\n");
#endif
      out3 = out;
    }
    
    memo;
    /* UPDATE POINT. */
    Ng(map)( *KV1_list_tl(in), out3 );
  }  
}

/* Recursively turns a list (named 'in') of key-sorted (key,value)
   pairs into the corresponding list (named 'kv_out') of
   (key,value-list) pairs, where each key is unique.  v_out points at
   the end of the current value-list. */
static void Ng(collect)(KV2_list_t   in,
                        K2_t*        key,
                        V2_list_t*   v_out,
                        KVL2_list_t* kvl_out) {

  if(in == NULL) {
    /* No more input.
       Terminate both output lists. */
    if(v_out) {
      *v_out = NULL;
    }
    *kvl_out = NULL;
  }
  else {
    /* Get next key/value pair. */
    
    KV2_t* kv = KV2_list_hd(in);
    K2_t  k  = kv->key;
    V2_t  v  = kv->val;

    if( key && Equalsf(k, *key) ) {
      /* Case: Same key:
         Append to v_out. */

      V2_list_t vs = memo(key, V2_list_cons(v));
      
      *v_out = vs;
      
      memo;
      Ng(collect)( *KV2_list_tl(in), key, V2_list_tl(vs), kvl_out );
    }
    else {
      /* Case: Different key:
         Terminate value-list.
         Append to kvl_out. */

      V2_list_t   vs = memo(key, V2_list_cons(v));
      KVL2_t*     kvl;
      KVL2_list_t kvls;
      
      memo {
        kvl = alloc(KVL2_t);
        kvl->key  = k;
        kvls = KVL2_list_cons( kvl );
      }

      kvl->vals = vs;
        
      if(v_out) {
        *v_out = NULL;
      }
      
      *kvl_out = kvls;

      memo;
      Ng(collect)( *KV2_list_tl(in), &(kvl->key),
                   V2_list_tl(vs), KVL2_list_tl(kvls) );
    }
  }
}

/* Printing lists for debugging purposes. */
static void KV2_list_print  (FILE* file, KV2_list_t  list);
static void KVL2_list_print (FILE* file, KVL2_list_t list);
static void KV3_list_print  (FILE* file, KV3_list_t  list);
  
/* Sorts the keys from the output of Map and collects values together
   with the same key (using collect, above). */
void Ng(shuffle)(KV2_list_t*  in,
                 KVL2_list_t* out)
{
  KV2_list_t in_sorted;

#if 0
  printf("shuffling begin\n");
  printf("  sorting begin\n");  
  KV2_list_print(stdout, *in);
#endif

  memo(fresh_scope) {
    Ng(mergesort)(in, &in_sorted);
  }

#if 0
  KV2_list_print(stdout, in_sorted);
  printf("  sorting end\n");
#endif
  
  Ng(collect)(in_sorted, NULL, NULL, out);

#if 0
  printf("shuffling end\n");
#endif
}


/* Invokes Reducef on each input key/value pair. */
void Ng(reduce)(KVL2_list_t in,
                KV3_list_t* out)
{
  if(in == NULL) {
    *out = NULL;
  }
  else {
    KVL2_t*      kvl = KVL2_list_hd(in);
    KV3_list_t* out2;
    KV3_list_t* out3;
    
    memo {
      out2 = alloc(KV3_list_t);
      out3 = Reducef( kvl->key, &(kvl->vals), out2 );
    }

    if(out2 != out3) {
      /* Case 1: The reduce function produced some elements. */
#if 0
      printf("reduce: got output\n");
#endif
      *out = *out2;
    }
    else {
      /* Case 2: The reduce function produced no elements. */
#if 0
      printf("reduce: no output\n");
#endif
      out3 = out;
    }
    
    memo;
    /* UPDATE POINT */
    KVL2_list_t next = *KVL2_list_tl(in);
    Ng(reduce)( next , out3 );
  }  
}

/* Puts everything together. */
void Ng(mapreduce)(KV1_list_t* in, KV3_list_t* out) {

  KV2_list_t  map_out;
  KVL2_list_t reduce_in;

  cut {
    Ng(map)( *in, &(map_out) );
    /*KV2_list_print(stdout, map_out);*/
  }  
  cut {
    Ng(shuffle)( &(map_out), &(reduce_in) );
    /*KVL2_list_print(stdout, reduce_in);*/
  }
  cut {
    /*KVL2_list_print(stdout, reduce_in);*/

    Ng(reduce)( reduce_in, out );

    /*KV3_list_print(stdout, *out );*/    
    /*KV3_list_print( stdout, *out );*/
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void KV2_list_print (FILE* file, KV2_list_t list) {
  KV2_list_t l = list;
  
  fprintf(file, "[");

  while(l != NULL) {
    KV2_list_t next = *KV2_list_tl(l);
    KV2_t*     kv   =  KV2_list_hd(l);

    fprintf(file, "(%s,%ld)%s", kv->key, kv->val,
            (next ? ", " : "") );
    l = next;
  }
  
  fprintf(file, "]\n");
}


static void V2_list_print (FILE* file, V2_list_t list) {
  V2_list_t l = list;
  
  fprintf(file, "[");

  while(l != NULL) {
    V2_list_t next = *V2_list_tl(l);
    V2_t      v    =  V2_list_hd(l);

    fprintf(file, "%ld%s", v, (next ? ", " : "") );
    l = next;
  }
  
  fprintf(file, "]");
}


static void KVL2_list_print (FILE* file, KVL2_list_t list) {
  KVL2_list_t l = list;
  
  fprintf(file, "[");

  while(l != NULL) {
    KVL2_list_t next = *KVL2_list_tl(l);
    KVL2_t*     kv   =  KVL2_list_hd(l);
    
    fprintf(file, "(%s, ", kv->key);
    V2_list_print(file, kv->vals);
    fprintf(file, ")%s", (next ? ", " : "") );
    l = next;
  }
  
  fprintf(file, "]\n");
}

static void KV3_list_print (FILE* file, KV3_list_t list) {
  KV3_list_t l = list;
  
  fprintf(file, "[");

  while(l != NULL) {
    KV3_list_t next = *KV3_list_tl(l);
    KV3_t*     kv   =  KV3_list_hd(l);
    
    fprintf(file, "(%ld,%ld)%s", kv->key, kv->val,
            (next ? ", " : "") );
    l = next;
  }
  
  fprintf(file, "]\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
