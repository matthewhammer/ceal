/* Matthew Hammer <hammer@mpi-sws.org>
   Reinhard Murz <munz@mpi-sws.org>
*/

/* Parameters to functor: */
#if  defined(Mapf)                 \
  && defined(Equalsf)              \
  && defined(Lessthanf)            \
  && defined(Reducef)              \
  /* Map Input */                  \
  && defined(K1_t)                 \
  && defined(V1_t)                 \
  /* Map Output / Reduce Input */  \
  && defined(K2_t)                 \
  && defined(V2_t)                 \
  /* Reduce Output */              \
  && defined(K3_t)                 \
  && defined(V3_t)
#else
#error Undefined functor parameters.
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  */
/* Please see the following:
 *   http://en.wikipedia.org/wiki/MapReduce#Logical_view
 */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Output of "preprocessing" / Input to mapper: */

typedef struct KV1_s { K1_t key; V1_t val; } KV1_t;
#define List_hd_t KV1_t* foreign_c
#define Ng(name) KV1_list_##name
#include "list_functor.c"
#undef List_hd_t
#undef Ng

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Output of mapper / Input to shuffler: */

typedef struct KV2_s { K2_t  key; V2_t  val; } KV2_t;
#define List_hd_t KV2_t* foreign_c
#define Ng(name) KV2_list_##name
#include "list_functor.c"
#undef List_hd_t
#undef Ng

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Output of shuffler / Input to reducer: */

#define List_hd_t V2_t foreign_c
#define Ng(name) V2_list_##name
#include "list_functor.c"
#undef List_hd_t
#undef Ng

typedef struct KVL2_s {
  K2_t      key;
  V2_list_t vals;
} KVL2_t;

#define List_hd_t KVL2_t* foreign_c
#define Ng(name) KVL2_list_##name
#include "list_functor.c"
#undef List_hd_t
#undef Ng

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Output of reducer: */

typedef struct KV3_s {
  K3_t key;
  V3_t val;
} KV3_t;

#define List_hd_t KV3_t* foreign_c
#define Ng(name) KV3_list_##name
#include "list_functor.c"
#undef List_hd_t
#undef Ng

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Client provides definition of:

   - Mapf       -- Maps key/value pairs to lists of key/value pairs.
   - Equalsf    -- Compares keys for equality (0 is not equal, 1 is equal)
   - Lessthanf  -- Compares keys for order (0 is greater than, 1 is less than or equal)
   - Reducef    -- Reduces a keyed list of values into a list of key/value pairs.

   with the following types (note these include the types declared above):
*/

/* Output list should be appended to out; its tail should be returned. */
KV2_list_t* Mapf (K1_t key, V1_t val, KV2_list_t* out);

/* Comparison used to sort by key, and to compare keys for equality. */
long Equalsf (K2_t kv1, K2_t kv2);
long Lessthanf (K2_t kv1, K2_t kv2);

/* Output list should be appended to out; its tail should be returned. */
KV3_list_t* Reducef (K2_t key, V2_list_t* vals,  KV3_list_t* out);
