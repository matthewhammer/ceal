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

/* Matthew Hammer */
/* <hammer@tti-c.org */
/*                         

 Total order data-structure : from "Two Simplified Algorithms for
   Maintaining Order in a List", Bender et. al -- Specifically, this
   is the "O(1) worst-case order data structure" from that paper,
   which does relabeling based on the "Tag-range relabeling algorithm"
   with an extra level of indirection to reduce O(log n) INSERT-time
   to O(1).
  
 API Overview:

   - CREATE() -> x
     create a new totalordering with one element, x
   
   - INSERT(x) -> y
     create an immediate successor y to x, return y

   - COMPARE(x, y) -> c,   c in {-1, 0, +1}
     follows same conventions as strcmp(3):
     (c is -1 if x < y, c is 0 if x = y, c is +1 if x > y)
     
   - ITERATE(x_1, x_k, f, a_0) -> ()
     say [x_1, x_k] = x_1, x_2, ..., x_k
     then for each i in {1, ..., k}
      let a_i = f(a_(i-1), data(x_i), x_i)
     finally, return a_k
     
     In other words, apply f to each x_i successively, carrying
     through an accumulator, initially a_0, which is finally given as
     the result.

   - REMOVE(x_1, x_k) -> ()
     Say I = [x_1, x_k] = x_1, x_2, ..., x_(k-1), x_k
     Let J = (x_1, x_k) = x_2, ..., x_(k-1)
     Then removes each x_i in J,
     leaving I = x_1, x_k (with no elements between x_1, x_k)

   - NEXT(x) -> y
     If x has an immediate successor, y, return y
     otherwise, x is the last element, return NULL
   
   Note on NULL elements:
     Although NEXT(x) may return NULL,     
     We assume that all API operations that expect an element x are
     given such an element and never given NULL.
     
 Implementation Overview:
   
   - 1st-level doubly linked list -- uses tag-range relabeling alg.
   
   - 2nd-level singly linked lists -- as children of each 1st-level
     node, these lists are constrained to contain at most (u / log u)
     nodes, where u is the number of total tags (u = 2^c for some
     constant c, -- e.g, c = 32 or 64, etc).
     
   When we insert, if we need more room in the tag space -- if there
   are no tags available where we'd like to insert -- we rebalance the
   2nd-level lists and, if necessary, overflow these lists into new
   1st-level nodes -- the new 1st level nodes are added similarly,
   where room for new nodes (in tag-space) is made by relabeling the
   1st-level list according to the algorithm mentioned above
   (tag-range relabeling).  This algoritm relabels the "smallest
   enclosing range not in overflow".  To find this range we find the
   tag-range density (the number of nodes in an interval of the
   ordering versus the number of tags in that interval) and compare
   this fraction to T^(-i) for ranges with 2^i tags. (T is picked
   between 1.0 and 2.0 and allows the "user" to tune the relabeling
   policy).  If the density is higher than T^(-i) we consider
   relabeling 2^(i+1) tags next.  This continues until we find a range
   not in overflow.
*/

#ifndef __TOTALORDER_H__
#define __TOTALORDER_H__

#include <inttypes.h>


typedef struct to_node2_s to_node_t;


to_node_t* totalorder_new(to_node_t* n1);

int totalorder_compare(to_node_t* n1, to_node_t* n2);

to_node_t* totalorder_succ(to_node_t* n);

/* Insert a successor in the total order of n,
   or create a new total order (when n == NULL). */
/* When n <> NULL, inserts succ as immediate successor to n */
/* When n == NULL, equivalent to totalorder_new(succ) */
to_node_t* totalorder_insert_succ(to_node_t* n, to_node_t* succ);

void totalorder_remove_interval(to_node_t* n1, to_node_t* n2);

void totalorder_remove_succ(to_node_t* n);


/* - - - - - - - - - - - - - - - - - - - - - - - -  */
/* Representation */

typedef unsigned long long to_label_t;  /* labels */
typedef struct to_node1_s to_node1_t;   /* parent nodes */
typedef struct to_node2_s to_node2_t;   /* child nodes */

extern to_label_t totalorder_relabel_count;

/* "Parent" nodes */
struct to_node1_s {
  to_node1_t* prev;
  to_node1_t* next;
  to_node2_t* children;
  to_label_t  label;
};

/* "Child" nodes */
struct to_node2_s {
  to_node2_t* next;
  to_node1_t* parent;
  to_label_t  label;
};

#endif
