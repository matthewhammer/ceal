/* Matthew Hammer <hammer@tti-c.edu> */

#include <assert.h>
#include <string.h>

#include "scalar_types.c"
#define SCALAR_DATA SCALAR_DOUBLE
#include "scalar_output.c"

#include "main.c"

typedef enum {PLUS,MINUS,TIMES,DIV} zwzr exptree_binop_t;

typedef enum {BINOP,VALUE} zwzr exptree_kind_t;

typedef double zwzr exptree_value_t;

struct exptree_node_s { exptree_kind_t __kind; };

typedef struct exptree_node_s exptree_node_t;

typedef exptree_node_t* owcr exptree_node_ptr_t;

typedef struct {
  exptree_node_t     __node;
  exptree_node_ptr_t __left;
  exptree_node_ptr_t __right;
  exptree_binop_t    __binop;
} exptree_binop_node_t;

typedef struct {
  exptree_node_t  __node;
  exptree_value_t __value;
} exptree_value_node_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - */
#ifndef CEAL_FOREIGN_IMMUTABLES
#define EXPTREE_FOREIGN /* empty */

static exptree_binop_node_t*
exptree_binop_node(exptree_binop_t binop) {
  exptree_binop_node_t* n = alloc(exptree_binop_node_t);
  n->__node.__kind = BINOP;
  n->__binop       = binop;
  return n;
}

static exptree_value_node_t*
exptree_value_node(exptree_value_t value) {
  exptree_value_node_t* n = alloc(exptree_value_node_t);
  n->__node.__kind = VALUE;
  n->__value       = value;
  return n;
}

#define exptree_node_kind(n) ({ n->__kind; })
#define exptree_node_value(n) ({ ((exptree_value_node_t*) n )->__value; })
#define exptree_node_binop(n) ({ ((exptree_binop_node_t*) n)->__binop; })
#define exptree_node_left(n) ({ & ((exptree_binop_node_t*) n )->__left; })
#define exptree_node_right(n) ({ & ((exptree_binop_node_t*) n )->__right; })

/* - - - - - - - - - - - - - - - - - - - - - - - - */
#else
#define EXPTREE_FOREIGN foreign_c

static exptree_binop_node_t*
exptree_binop_node_init(exptree_binop_node_t* n, exptree_binop_t binop) EXPTREE_FOREIGN {
  n->__node.__kind = BINOP;
  n->__binop       = binop;
  return n;
}

#define exptree_binop_node(binop) ({                      \
  exptree_binop_node_t* n = alloc(exptree_binop_node_t);  \
  exptree_binop_node_init(n, binop); })

static exptree_value_node_t*
exptree_value_node_init(exptree_value_node_t* n, exptree_value_t value) EXPTREE_FOREIGN {
  n->__node.__kind = VALUE;
  n->__value       = value;
  return n;
}

#define exptree_value_node(value) ({                      \
  exptree_value_node_t* n = alloc(exptree_value_node_t);  \
  exptree_value_node_init(n, value); })


exptree_kind_t exptree_node_kind(exptree_node_t* n) EXPTREE_FOREIGN {
  return n->__kind;
}

exptree_value_t exptree_node_value(exptree_node_t* n) EXPTREE_FOREIGN {
  return ((exptree_value_node_t*) n )->__value;
}

exptree_binop_t exptree_node_binop(exptree_node_t* n) EXPTREE_FOREIGN {
  return ((exptree_binop_node_t*) n)->__binop;
}

exptree_node_t** exptree_node_left(exptree_node_t* n) EXPTREE_FOREIGN {
  return & ((exptree_binop_node_t*) n )->__left;
}

exptree_node_t** exptree_node_right(exptree_node_t* n) EXPTREE_FOREIGN {
  return & ((exptree_binop_node_t*) n )->__right;
}

#endif
/* - - - - - - - - - - - - - - - - - - - - - - - - */

static void
exptree_fprint(FILE* file, exptree_node_t* n)
{
  if( exptree_node_kind( n ) == VALUE ) {
    fprintf(file, "%g", exptree_node_value( n ) );
  }
  else if( exptree_node_kind( n ) == BINOP ) {

    fprintf(file, "(");
    
    exptree_fprint( file, *exptree_node_left( n ) );

    exptree_binop_t binop = exptree_node_binop( n );

    if      ( binop == PLUS )  { fprintf(file, " + "); }
    else if ( binop == MINUS ) { fprintf(file, " - "); }
    else if ( binop == TIMES ) { fprintf(file, " * "); }
    else if ( binop == DIV )   { fprintf(file, " / "); }
    else    abort();

    exptree_fprint( file, *exptree_node_right( n ) );

    fprintf(file, ")");
  }      
}

static int
exptree_random_rec(int num_nodes, exptree_node_ptr_t* p) {
  if( num_nodes > 2 ) {
    exptree_binop_node_t* node = exptree_binop_node( rand() % 4 );
    int n = num_nodes - 1;
    int n_l = exptree_random_rec( n / 2,   exptree_node_left( node ) );
    int n_r = exptree_random_rec( n - n_l, exptree_node_right( node ) );
    *p = node;
    return (n_l + n_r + 1);
  }
  else if( num_nodes == 1 ) {
    exptree_value_node_t* n = exptree_value_node( random_double() );
    *p = n;
    return 1;
  }
  else if ( num_nodes == 2 ) {
    return exptree_random_rec (3, p);
  }
  abort();
  return 0;
}

static void
exptree_eval_dps(exptree_node_t* n, double owcr * result)
{  
  if( exptree_node_kind( n ) == VALUE ) {
    *result = exptree_node_value ( n );
  }
  else if( exptree_node_kind( n ) == BINOP ) {
    double owcr l;
    double owcr r;
    
    exptree_eval_dps( *exptree_node_left( n ),  &l);
    exptree_eval_dps( *exptree_node_right( n ), &r);

    exptree_binop_t binop = exptree_node_binop( n );

    if( binop == PLUS ) {
      *result = l + r;
    }
    else if ( binop == MINUS ) {
      *result = l - r;
    }
    else if ( binop == TIMES ) {
      *result = l * r;
    }
    else if ( binop == DIV ) {
      *result = l / r;
    }
    else
      abort();
  }
}

static exptree_value_t
exptree_eval_direct(exptree_node_t* n)
{
  if( exptree_node_kind( n ) == VALUE ) {
    return exptree_node_value ( n );
  }
  else if( exptree_node_kind( n ) == BINOP ) {
    double l = exptree_eval_direct( *exptree_node_left( n ) );
    double r;
    memo {
      r = exptree_eval_direct( *exptree_node_right( n ) );
    }
    exptree_binop_t binop = exptree_node_binop( n );
    
    if( binop == PLUS ) {
      return l + r;
    }
    else if ( binop == MINUS ) {
      return l - r;
    }
    else if ( binop == TIMES ) {
      return l * r;
    }
    else if ( binop == DIV ) {
      return l / r;
    }
  }
  abort();
  return 0.0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Exptrees as Input. */

exptree_node_ptr_t   exptree_input; /* Input. */
long                 exptree_leafc; /* # of leaves. */
long                 exptree_leafi; /* Current leaf index. */
long                 exptree_change_size;

exptree_node_ptr_t*zwzr*zwzr exptree_leafv; /* Vector of leaf pointers. */
exptree_node_t*    zwzr*zwzr exptree_leafv_saved;

static long num_leaves(exptree_node_t* n) {
  if ( exptree_node_kind( n ) == VALUE ) {
    return 1;
  }
  else {
    return
      num_leaves( *exptree_node_left( n ) ) +
      num_leaves( *exptree_node_right( n ) );
  }
}

static void find_leaves_rec(exptree_node_ptr_t* n_ptr, exptree_node_t* n) {  
  if ( exptree_node_kind( n ) == VALUE ) {
    assert ( exptree_leafi < exptree_leafc );
    exptree_leafv       [ exptree_leafi ] = n_ptr; /* Save the leaf pointer. */
    exptree_leafv_saved [ exptree_leafi ] = n; /* Save the leaf itself. */
    exptree_leafi += 1;
  }
  else {
    exptree_binop_node_t* bn = (exptree_binop_node_t*) n;
    find_leaves_rec( exptree_node_left(bn),  *exptree_node_left(bn));
    find_leaves_rec( exptree_node_right(bn), *exptree_node_right(bn));
  }
}

static void find_leaves(exptree_node_ptr_t* n_ptr, exptree_node_t* n) {

  long num = num_leaves( n );

  exptree_leafv = malloc( num * (sizeof(exptree_node_ptr_t*)) );
  memset(exptree_leafv, 0, num * (sizeof(exptree_node_ptr_t*)));

  exptree_leafv_saved = malloc( num * (sizeof(exptree_node_t*)) );
  memset(exptree_leafv_saved, 0, num * (sizeof(exptree_node_t*)));

  exptree_leafi = 0;
  exptree_leafc = num;
  
  find_leaves_rec( n_ptr, n );
}


void cealtesthook_input_generate(long size) {
  exptree_random_rec( size, & exptree_input );
  find_leaves( & exptree_input, exptree_input );
}

void cealtesthook_input_print(FILE* file) {
  exptree_fprint( file, exptree_input );
}

void cealtesthook_input_iter_begin(long change_size) {
  exptree_leafi = 0;
  exptree_change_size = change_size;
}

void cealtesthook_input_iter_next() {

  exptree_leafi += exptree_change_size;

  if( exptree_leafi > exptree_leafc )
    exptree_leafi = exptree_leafc;
}

int cealtesthook_input_iter_isdone() {
  return exptree_leafi == exptree_leafc;
}

void cealtesthook_input_iter_change() {
  int i = 0;
  while( i < exptree_change_size &&
         exptree_leafi + i < exptree_leafc ) {
  
    /* Choose a new (random) leaf. */
    *(exptree_leafv[ exptree_leafi + i ]) =
      exptree_value_node ( random_double() );

    i++;
  }
}

void cealtesthook_input_iter_revert() {
  int i = 0;
  while( i < exptree_change_size &&
         exptree_leafi + i < exptree_leafc ) {

    /* Kill the new leaf. */
    kill(*(exptree_leafv[ exptree_leafi + i]));

    /* Restore the original one. */
    *(exptree_leafv [ exptree_leafi + i] ) =
      exptree_leafv_saved [ exptree_leafi + i];

    i++;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* Exptree Evaluation as a Benchmark. */


static void
exptree_eval_core(exptree_node_t* n, double owcr* result) {
#if 0
  exptree_eval_dps(n, result);
#else
  memo(fresh_scope);    
  *result = exptree_eval_direct(n);
#endif  
}

void cealtesthook_run_core() {
  core(exptree_eval_core)(exptree_input, &scalar_output_core);
}

void cealtesthook_run_verf() {
  exptree_eval_dps(exptree_input, &scalar_output_verf);
}
