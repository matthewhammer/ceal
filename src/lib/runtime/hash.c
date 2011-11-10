/*
Copyright 2008-2011 
Matthew A. Hammer <hammer@mpi-sws.org>

This file is part of the CEAL language implementation (CEAL for short).

CEAL is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

CEAL is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with CEAL.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Matthew Hammer <hammer@tti-c.org> */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "hash.h"

#include "hash_util.h"

/*
  Hash Functions.

  Some of the hashing code found here and in "hash_util.h" is due to
  Bob Jenkins.  It was derived from the following (public domain) C
  code he posted online here:
  
  -- http://burtleburtle.net/bob/c/lookup3.c  (32-bit)
  -- http://burtleburtle.net/bob/c/lookup8.c  (64-bit)

  The code for hashing a single 32-bit or 64-bit integer is due to
  Thomas Wang.  This code orginally appeared online here:

  -- http://www.cris.com/~Ttwang/tech/inthash.htm  
*/


/* (Thomas Wang) */
#if CEAL_HASH_BITS==32
uint32_t
hash_oneword_32(uint32_t a)
{
  a = (a+0x7ed55d16) + (a<<12);
  a = (a^0xc761c23c) ^ (a>>19);
  a = (a+0x165667b1) + (a<<5);
  a = (a+0xd3a2646c) ^ (a<<9);
  a = (a+0xfd7046c5) + (a<<3);
  a = (a^0xb55a4f09) ^ (a>>16);
  return a;
}
#endif

/* (Thomas Wang) */
#if CEAL_HASH_BITS==64
uint64_t
hash_oneword_64(uint64_t a) {
  a = (~a) + (a << 21); // a = (a << 21) - a - 1;
  a = a ^ (a >> 24);
  a = (a + (a << 3)) + (a << 8); // a * 265
  a = a ^ (a >> 14);
  a = (a + (a << 2)) + (a << 4); // a * 21
  a = a ^ (a >> 28);
  a = a + (a << 31);
  return a;
}
#endif

uintptr_t
hash_oneword(uintptr_t word) {
#if   CEAL_HASH_BITS==32
  return hash_oneword_32(word);
#elif CEAL_HASH_BITS==64
  return hash_oneword_64(word);
#else
  abort();
#endif
}

/*
  hash_buffer_32 -- hash a buffer of data into a 32-bit value.
  Assumes that (bytec % 4 == 0).  From Bob Jenkin's C source code:
  "lookup3.c".
*/
#if CEAL_HASH_BITS==32
uint32_t
hash_buffer_32(unsigned char* bytes,
               uint32_t bytec,
               uint32_t hashin)
{
  const uint32_t* k;
  uint32_t  a,b,c;
  size_t    length;
  
  /* length is the number of words we've been given. */
  length = bytec / 4;
  assert((bytec % 4) == 0);

  /* initialize */
  k = (uint32_t*) bytes;
  a = b = c = 0xdeadbeef + (((uint32_t)length)<<2) + hashin;

  while (length > 3)
  {
    a += k[0];
    b += k[1];
    c += k[2];
    mix(a,b,c);
    length -= 3;
    k += 3;
  }

  switch(length)
  { 
  case 3 : c+=k[2];
  case 2 : b+=k[1];
  case 1 : a+=k[0];
    final(a,b,c);
  case 0:
    break;
  }

  return c;
}
#endif

/*
  hash_buffer_64 -- hash a buffer of data into a 64-bit value.  length
  is measured in bytes.  From Bob Jenkin's C source code: "lookup8.c".
*/
#if CEAL_HASH_BITS==64
uint64_t
hash_buffer_64(unsigned char* bytes,
               uint64_t bytec,
               uint64_t hashin)
{
  unsigned char* k;
  uint64_t a,b,c,len,length;

  /* Set up the internal state */
  length = bytec;
  len = bytec;
  k   = bytes;
  a = b = hashin;                         /* the previous hash value */
  c = 0x9e3779b97f4a7c13LL; /* the golden ratio; an arbitrary value */

  /* handle most of the key */
  while (len >= 24)
  {
    a += (k[0]        +((uint64_t)k[ 1]<< 8)+((uint64_t)k[ 2]<<16)+((uint64_t)k[ 3]<<24)
     +((uint64_t)k[4 ]<<32)+((uint64_t)k[ 5]<<40)+((uint64_t)k[ 6]<<48)+((uint64_t)k[ 7]<<56));
    b += (k[8]        +((uint64_t)k[ 9]<< 8)+((uint64_t)k[10]<<16)+((uint64_t)k[11]<<24)
     +((uint64_t)k[12]<<32)+((uint64_t)k[13]<<40)+((uint64_t)k[14]<<48)+((uint64_t)k[15]<<56));
    c += (k[16]       +((uint64_t)k[17]<< 8)+((uint64_t)k[18]<<16)+((uint64_t)k[19]<<24)
     +((uint64_t)k[20]<<32)+((uint64_t)k[21]<<40)+((uint64_t)k[22]<<48)+((uint64_t)k[23]<<56));
    mix(a,b,c);
    k += 24; len -= 24;
  }

  /* handle the last 23 bytes */
  c += length;
  switch(len) /* all the case statements fall through */
  {
  case 23: c+=((uint64_t)k[22]<<56);
  case 22: c+=((uint64_t)k[21]<<48);
  case 21: c+=((uint64_t)k[20]<<40);
  case 20: c+=((uint64_t)k[19]<<32);
  case 19: c+=((uint64_t)k[18]<<24);
  case 18: c+=((uint64_t)k[17]<<16);
  case 17: c+=((uint64_t)k[16]<<8);
    /* the first byte of c is reserved for the length */
  case 16: b+=((uint64_t)k[15]<<56);
  case 15: b+=((uint64_t)k[14]<<48);
  case 14: b+=((uint64_t)k[13]<<40);
  case 13: b+=((uint64_t)k[12]<<32);
  case 12: b+=((uint64_t)k[11]<<24);
  case 11: b+=((uint64_t)k[10]<<16);
  case 10: b+=((uint64_t)k[ 9]<<8);
  case  9: b+=((uint64_t)k[ 8]);
  case  8: a+=((uint64_t)k[ 7]<<56);
  case  7: a+=((uint64_t)k[ 6]<<48);
  case  6: a+=((uint64_t)k[ 5]<<40);
  case  5: a+=((uint64_t)k[ 4]<<32);
  case  4: a+=((uint64_t)k[ 3]<<24);
  case  3: a+=((uint64_t)k[ 2]<<16);
  case  2: a+=((uint64_t)k[ 1]<<8);
  case  1: a+=((uint64_t)k[ 0]);
    /* case 0: nothing left to add */
  }
  mix(a,b,c);
  return c;
}
#endif

uintptr_t
hash_buffer(unsigned char* bytes, uintptr_t bytec, uintptr_t hashin) {
#if   CEAL_HASH_BITS==32
  return hash_buffer_32(bytes, bytec, hashin);
#elif CEAL_HASH_BITS==64
  return hash_buffer_64(bytes, bytec, hashin);
#else
  abort();
  return 0;
#endif
}

