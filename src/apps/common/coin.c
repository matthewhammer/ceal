#ifndef __COIN_H__
#define __COIN_H__

/* Coins -- Allow self-adjusting programs to make random decisions
   while maintaining necessary stability.

   Once a coin has been constructed (which is usually random), the
   user can "flip" a coin deterministically by supplying a word-value
   [x] (see: [coin_flip]).  The outcome of a flip given an [x] will
   either be 0 or 1.  Given the same [x] and the same coin, the
   outcome will always be the same.  Since a coin is deterministic,
   the program using such a coin is stable in the sense than
   re-evaluation of any series of identitical flips will yield
   identitical results.

   Each coin also carries a bias which controls the expected outcome
   of coin flips (expectation taken over all values of [x]).  A bias
   of 1 isn't interesting, any coin flip will yield 0. A bias of 2
   will yield a fair coin where the outcome of a flip is equally
   likely to be 0 or 1.  A biased coin is one where the bias is
   greater than 2, in which case the outcome of a coin flip is more
   likely to be 1 than 0.
*/

#ifdef CEAL_FOREIGN_IMMUTABLES
#define COIN_FOREIGN foreign_c
#else
#define COIN_FOREIGN
#endif

typedef struct coin_s coin_t;

/* TODO: do we need the following? */
extern long hash_oneword(long x);

#define coin_fair coin_biased_2

coin_t* coin_biased_2();
coin_t* coin_biased_4();
coin_t* coin_biased_8();
coin_t* coin_biased_16();

/* Coin flipping -- given a word [x], flips the coin deterministically
   for [x] and returns either 1 or 0.  If the coin is biased, the
   expected number of 0's (expectation taken over all possible values
   for [x]) is _less_ than the number of 1's.  Higher biases will
   yield lower expectations of flipping a 0. */
long coin_flip(coin_t* coin, void* x) COIN_FOREIGN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

struct coin_s {
  long zwzr seed1;
  long zwzr seed2;
  long zwzr bias;
};

coin_t* coin_init(coin_t* coin, long bias) COIN_FOREIGN {          \
  coin->seed1 = rand();                                            \
  coin->seed2 = rand();                                            \
  coin->bias  = bias;                                              \
  return coin;                                                     \
}

#define coin_biased_2()  ({coin_init(alloc(coin_t), 2) ;})
#define coin_biased_4()  ({coin_init(alloc(coin_t), 4) ;})
#define coin_biased_8()  ({coin_init(alloc(coin_t), 8) ;})
#define coin_biased_16() ({coin_init(alloc(coin_t), 16);})

/* For mix macro: */
#include "../../lib/runtime/hash_util.h"



long coin_flip(coin_t* coin, void* x) COIN_FOREIGN {
  long a = coin->seed1;
  long b = coin->seed2;
  long c = (long) x;

  /* Mix the three words into [c] */
  mix(a, b, c);
  
  /* Modulus with the bias */
  return c % coin->bias;
}


#endif
