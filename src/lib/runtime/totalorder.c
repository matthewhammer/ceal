/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: totalorder.c
Author(s): Matthew A. Hammer <hammer@mpi-sws.org>

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

#include <assert.h>
#include <stdlib.h>
#include "basemm.h"
#include "totalorder.h"
#include "trace.h"

#define TO_T           (1.4)
#define TO_LABEL_BITS  ((sizeof(to_label_t)) * 8)
#define TO_LIST_SIZE   (TO_LABEL_BITS)
#define TO_MAX_LABEL   ((to_label_t) 1 << (TO_LABEL_BITS - 1))
#define TO_GAP_SIZE    (TO_MAX_LABEL / TO_LIST_SIZE)
#define TO_END_LABEL   (TO_MAX_LABEL - TO_GAP_SIZE)

#define TO_ON(x) x
#define TO_OFF(x) ;

#define TO_STAT(x)   TO_OFF(x)
#define TO_CHECKS(x) TO_OFF(X)

to_label_t totalorder_relabel_count = 0;

void totalorder__balance1(to_node1_t* n);
void totalorder__balance2(to_node1_t* cur1, to_node2_t* cur2);

to_node1_t* totalorder__alloc1() {
  to_node1_t* n = (to_node1_t*) basemm_malloc(sizeof(to_node1_t));
  return n;
}

/*
to_node2_t* totalorder__alloc2() {
  to_node2_t* n = (to_node2_t*) basemm_malloc(sizeof(to_node2_t));
  return n; 
}
*/

void totalorder__free1(to_node1_t* n) {
#ifdef CEAL_DEBUG
  n->children = NULL;
  n->prev     = NULL;
  n->next     = NULL;
#endif
  basemm_free(sizeof(to_node1_t), n);
}

void totalorder__free2(to_node2_t* n) {
#ifdef CEAL_DEBUG
  n->parent = NULL;
  n->next = NULL;
  n->label = 0xdeadbeef;
#endif
  basemm_free(sizeof(to_node2_t), n);
}

to_node_t* totalorder_new(to_node_t* n2) {
  to_node1_t* n1 = totalorder__alloc1();

  n1->prev = n1;
  n1->next = n1;
  n1->children = n2;
  n1->label = 0x0;

  n2->next = NULL;
  n2->label = 0x0;
  n2->parent = n1;
  
  return n2;
}

int totalorder_compare(to_node_t* n1, to_node_t* n2) {
  /* TODO: what is the right semantics for comparing to NULL ? */
  /* assert(n1 && n2); */
  to_node1_t* n1_parent = n1->parent;
  to_node1_t* n2_parent = n2->parent;

  if(!n1_parent && !n2_parent)
    return 0;

  else if(!n1_parent)
    return -1;

  else if(!n2_parent)
    return 1;
  
  else {    
    to_label_t p1_label = n1_parent->label;
    to_label_t p2_label = n2_parent->label;

    /* test parents' labels -- if equal, test labels of level-2 nodes */
    if(p1_label < p2_label)
      return -1;

    else if(p1_label > p2_label)
      return 1;
    
    else {
      to_label_t n1_label = n1->label;
      to_label_t n2_label = n2->label;
      
#ifdef CEAL_DEBUG
      assert(n1->parent == n2->parent);
#endif
      
      if(n1_label < n2_label)
        return -1;
      
      else if(n1_label > n2_label)
        return 1;

      else {
#ifdef CEAL_DEBUG
        assert(n1 == n2);
#endif        
        return 0;
      }
    }
  }
}

to_node_t* totalorder_succ(to_node_t* n) {
  if(n->next) return n->next;
  else {
    to_node_t* m = n->parent->next->children;
    if(totalorder_compare(n, m) < 0)
      return m;
    else
      return NULL;
  }
}

to_node_t* totalorder_insert_succ(to_node_t* n, to_node_t* new_node) {

  if(n == NULL) {
    return totalorder_new(new_node);
  }
  
  else {
    to_label_t label_of_next;
  
    if(n->next) label_of_next = n->next->label;
    else        label_of_next = TO_MAX_LABEL;
    
    /* Initialize the new node -- note: new label is half-way between
       that of n and n->next.  However, Bender et. al. show that this
       choice is mostly arbitrary. */
    new_node->label = (n->label + label_of_next) >> 1;
    new_node->next = n->next;
    new_node->parent = n->parent;    
    n->next = new_node;
    
    /* if we've run out of labels, relabel the 2nd-level list */
    if(n->label == new_node->label) {
      totalorder__balance2(n->parent, n->parent->children);
      assert(n->label != new_node->label);
    }
    return new_node;
  }
}

void totalorder_remove_succ(to_node_t* n) {
  to_node_t* node_out = n->next;
  n->next = node_out->next;
  totalorder__free2(node_out);
}

void totalorder__check1(to_node1_t* p) {
  to_node1_t* first = p;
  int wrapped = 0;
  
  /* printf("level-1 nodes:\n"); */
  while(1) {    
    /*
      printf("[%p %llu] -> [%p %llu]\n",
             p, (long long unsigned) p->label,
             p->next, (long long unsigned) p->next->label);
    */
    
    if(p->next == first)
      break;
    else if(p->label > p->next->label && !wrapped)
      wrapped = 1;
    else
      assert(p->label < p->next->label);
    
    p = p->next; 
  }
}

void totalorder__balance1(to_node1_t* n) {
  to_node1_t* lo = n;
  to_node1_t* hi = n;
  to_label_t lo_label;
  to_label_t hi_label;
  to_label_t range_count = 1;
  to_label_t label_mask = 1;
  
  /* invariant: label_mask == ((# of tags between lo and hi) - 1) */
  
  /* (i): Find the "smallest enclosing range in overflow" */
  {
    double tau = 1.0 / TO_T;
    to_label_t base_label = n->label;

    while(1) {
      double density;
      lo_label = (base_label & (~label_mask));
      hi_label = (lo_label | label_mask);
      
      /* (a) : move lo left (down in labels) */
      while(lo->prev->label >= lo_label &&
            lo->prev->label <= lo->label  ) {
        lo = lo->prev;
        ++ range_count;
      }
      
      /* (b) : move hi right (up in labels) */
      while(hi->next->label <= hi_label &&
            hi->next->label >= hi->label) {
        hi = hi->next;
        ++ range_count;
      }
      
      density = (double) range_count / (double) (label_mask + 1);
      if(density < tau) {
        /* we've found the smallest tag-range that is not in overflow,
           we are setup to relabel (balance) it by using [lo],
           [lo_label], [hi], [label_mask] and [range_count].  */
        if(0) {
          /*
          printf("base_label  = %u = %x\n", base_label, base_label);
          printf("lo_label    = %u = %x\n", lo_label, lo_label);
          printf("hi_label    = %u = %x\n", hi_label, hi_label);
          printf("label_mask  = %u = %x\n", label_mask, label_mask);
          printf("range_count = %u\n", range_count);
          printf("incr        = %u\n", (label_mask + 1) / range_count);
          */
        }
        break;
      }
      else {
        /* setup for next iteration: the size of the range (measured
           by tags) doubles. tau is divided by T. the label mask,
           which corresponds to the size of range (in tags) minus one,
           gets 1 bit longer (moving towards the most-sig-bit). */
        label_mask = (label_mask << 1) | 1;
        tau /= TO_T;
#ifdef CEAL_DEBUG
        assert(label_mask < TO_MAX_LABEL);
#endif
      }
    }
  }
  
  /* (ii): Given tag- and node-ranges found above, relabel evenly */
  {
    to_node1_t* cur = lo;
    to_label_t label = lo_label;
    to_label_t incr = ((label_mask + 1) / range_count);    
    while(1) {
      cur->label = label;        
      if(cur == hi) break;
      cur = cur->next;
      label += incr;

      TO_STAT(++ totalorder_relabel_count);
    }
  }
}

static to_node1_t*
totalorder__mklevel1node(to_node1_t* prev, to_node2_t* children) {
  to_node1_t* new_node = totalorder__alloc1();
  to_label_t prev_label = prev->label;
  to_label_t next_label =
    (prev->next->label > prev_label) ?
    prev->next->label : prev_label + 2;
  
  /* initialize new_node */
  new_node->prev = prev;
  new_node->next = prev->next;
  new_node->label = prev_label;
  new_node->children = children;

  /* check the current structure before changing it */
  TO_CHECKS(totalorder__check1(prev));
  
  /* patch new_node into the doubly linked list */
  prev->next = new_node;
  new_node->next->prev = new_node;

  /* rebalance 1st-level list, if necessary, otherwise just use the
     "middle" available label. */
  if(prev_label + 1 == next_label)
    totalorder__balance1(new_node);
  else
    new_node->label = (prev_label + next_label) >> 1;  
  
  TO_CHECKS(totalorder__check1(prev));
  return new_node;
}

void totalorder__balance2(to_node1_t* cur1, to_node2_t* cur2) {
  while(cur2) {
    to_node2_t* cur2_ = NULL;
    to_label_t num = 0;
    
    while(num < TO_END_LABEL && cur2) {
      /* relabel/reparent cur2 */
      cur2->parent = cur1;
      cur2->label = num;

      /* advance variables: num, cur2, cur2_ */
      num += TO_GAP_SIZE;
      cur2_ = cur2;
      cur2 = cur2->next;
      
      TO_STAT(++ totalorder_relabel_count);
    } 

    if(cur2) {
      /* still more nodes in cur2, so make a new top-level node as
         cur1 and terminate old cur2 with NULL. */
      cur1 = totalorder__mklevel1node(cur1, cur2);
      cur2_->next = NULL;
    }
  }
}

void totalorder_remove_interval(to_node_t* n1, to_node_t* n2)
{
#ifdef CEAL_DEBUG
  assert(n1 && n2);
  assert(totalorder_compare(n1, n2) <= 0);
#endif
  {
    to_node_t* i = n1->next;
    to_node1_t* i_parent = n1->parent;  
  
    while(1) {
      /* case 1: handle end of a list of children */
      if(!i) {
        to_node1_t* i_parent0 = i_parent;
        
        i_parent = i_parent->next;
        i = i_parent->children;
        
        if(i_parent0 != n1->parent)
          totalorder__free1(i_parent0);
        continue;
      }
      /* case 2: handle end of interval (or end of timestamps by
         detecting timestamp wrap-around) */
      else if( totalorder_compare(n1, i) < 0 &&
               totalorder_compare(i, n2) < 0 ) {
        to_node_t* i0 = i;
        i = i->next;
        
        /* Call back into CEAL RT. */
        ceal_time_undo(i0);
        
        /*totalorder__free2(i0); */
        continue;
      }
      else
        break;
    }
    
    /* lastly, fixup any dangling pointers.  There are two cases: (1) n1
       and n2 are in the same child list, (2) n1 and n2 have different
       parents.  */
    if(n1 != n2) {
      if(n1->parent == n2->parent) {
        n1->next = n2;
      }
      else {
        n1->next = NULL;
        n1->parent->next = n2->parent;
        n2->parent->prev = n1->parent;
        n2->parent->children = n2;
      }
    }
  }
}
