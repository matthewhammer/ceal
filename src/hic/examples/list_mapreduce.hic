/<<
requires "list.hic"
requires "list_mergesort.hic"
  
module List_mapreduce = {

  module type KVs = {
    type k1_t; type v1_t;
    type k2_t; type v2_t;
    type k3_t; type v3_t;
  }

  module Decls
  ( KVs : Kvs )
  ( Q : { qual q } )
  =
  {
    include Kvs
    include Q
    
    >>/

    typedef struct kv1_s { k1_t key; v1_t val; } kv1_t;
    typedef struct kv2_s { k2_t key; v2_t val; } kv2_t;
    typedef struct kv3_s { k3_t key; v3_t val; } kv3_t;

    /<<
    
    module kv1_list  = List_Make({ type hd_t = kv1_t*  qual q = q })
    module kv2_list  = List_Make({ type hd_t = kv2_t*  qual q = q })
    module v2_list   = List_Make({ type hd_t = v2_t*   qual q = q })
    module kv3_list  = List_Make({ type hd_t = kv3_t*  qual q = q })
  }
  
  module Make
  ( KVs : KVs )
  ( D   : Decls ( KVs ) )
  ( F   : {
    open D ;
    val compare : ( k2_t , k2_t ) -> int ;
    val map     : ( k1_t , v1_t ,      kv2_list_t* ) -> kv2_list_t* ;
    val reduce  : ( k2_t , v2_list_t , kv3_list_t* ) -> kv3_list_t* ;
  } )
  =
  {
    open D ;    

    >>/

    typedef struct kvl2_s {
      k2_t      key ;
      v2_list_t vals ;
    } kvl2_t;    

    /<<
    
    module kvl2_list = List ({
        type hd_t = kvl2_t* ;
        qual q = q ;
      })
    
    include Mergesort({
        open Kv2_list ;
        val compare = F_compare ;
      })

    >>/

    /* Invokes F_map on each input key/value pair. */
    static void map(kv1_list_t  in,
                    kv2_list_t* out)
    {
      if(in == NULL) {
        *out = NULL;
      }
      else {
        kv1_t*        kv = kv1_list_hd(in);
        kv2_list_t* out2;
        kv2_list_t* out3;
        
        memo {
          out2 = alloc( kv2_list_t );
          out3 = F_map( kv->key, kv->val, out2 );
          *out = *out2;
        }
        
        memo;
        map( *kv1_list_tl(in), out3 );
      }  
    }

    /* Recursively turns a list (named 'in') of key-sorted (key,value)
       pairs into the corresponding list (named 'kv_out') of
       (key,value-list) pairs, where each key is unique.  v_out points
       at the end of the current value-list. */
    static void collect(kv2_list_t   in,
                        K2_t*        key,
                        v2_list_t*   v_out,
                        kvl2_list_t* kv_out) {
      
      if(in == NULL) {
        /* No more input.
           Terminate both output lists. */
        if(v_out) {
          *v_out = NULL;
        }
        *kv_out = NULL;
      }
      else {
        /* Get next key/value pair. */
        
        kv2_t* kv = kv2_list_hd(in);
        K2_t*  k  = kv->key;
        v2_t*  v  = kv->val;
        
        if( F_compare (k, key) == 0 ) {
          /* Case: Same key:
             Append to v_out. */
          
          v2_list_t vs = memo( v2_list_cons(v) );
          
          *v_out = vs;
          
          collect( *kv2_list_tl(in), k,
                   v2_list_tl(vs), kvl2_list_tl(kv_out) );
        }
        else {
          /* Case: Different key:
             Terminate value-list.
             Append to kv_out. */
          
          v2_list_t vs = memo( v2_list_cons(v) );
          kvl2_t* kvl;
          
          memo {
            kvl = alloc(kvl2_t);
            kvl->key  = k;
            kvl->vals = vs;
          }
          
          if(v_out) {
            *v_out = NULL;
          }
          
          *kv_out = kvl;
          
          collect( *kv2_list_tl(in), k,
                   v2_list_tl(vs), kvl2_list_tl(kv_out) );
        }
      }
    }

    /* Sorts the keys from the output of map and collects values
       together with the same key (using collect, above). */
    void shuffle( kv2_list_t*  in,
                  kvl2_list_t* out )
    {
      kv2_list_t in_sorted;
      
      mergesort( in, &in_sorted );
      collect( in_sorted, NULL, NULL, out );
    }
    
    
    /* Invokes F_reduce on each input key/value pair. */
    void reduce( kvl2_list_t in,
                 kv3_list_t* out )
    {
      if(in == NULL) {
        *out = NULL;
      }
      else {
        kvl2_t*      kvl = kvl2_list_hd(in);
        kv3_list_t* out2;
        kv3_list_t* out3;
        
        memo {
          out2 = alloc(kv3_list_t*);
          out3 = F_reduce( kvl->key, kvl->vals, out2 );
          *out = *out2;
        }
        
        memo;
        reduce( *kvl2_list_tl(in), out3 );
      }  
    }
    
    /* Puts everything together. */
    void mapreduce( kv1_list_t* in, kv3_list_t* out ) {
      
      kv2_list_t map_out;
      kv2_list_t reduce_in;
      
      cut {
        map( in, &(map_out) );
      }  
      cut {
        shuffle( &(map_out), &(reduce_in) );
      }
      cut {
        reduce( &(reduce_in), out );
      }
    }
  }
}
