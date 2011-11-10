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

/*
  Matthew Hammer <hammer@mpi-sws.org>
*/

#ifndef __CEAL_MODREF_RING_C__
#define __CEAL_MODREF_RING_C__

#include "trace.h"

#define Tv   void*
#define Fv   "%p"
#define S(x) ceal_## x ##_ring_p_s
#define T(x) ceal_## x ##_ring_p_t
#define F(x) ceal_## x ##_ring_p
#define E(x) CEAL_## x ##_ring_P
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#define Tv   int
#define Fv   "%d"
#define S(x) ceal_## x ##_ring_i_s
#define T(x) ceal_## x ##_ring_i_t
#define F(x) ceal_## x ##_ring_i
#define E(x) CEAL_## x ##_ring_I
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#define Tv   unsigned int
#define Fv   "%ud"
#define S(x) ceal_## x ##_ring_ui_s
#define T(x) ceal_## x ##_ring_ui_t
#define F(x) ceal_## x ##_ring_ui
#define E(x) CEAL_## x ##_ring_UI
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#define Tv   long
#define Fv   "%ld"
#define S(x) ceal_## x ##_ring_l_s
#define T(x) ceal_## x ##_ring_l_t
#define F(x) ceal_## x ##_ring_l
#define E(x) CEAL_## x ##_ring_L
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#define Tv   unsigned long
#define Fv   "%ld"
#define S(x) ceal_## x ##_ring_ul_s
#define T(x) ceal_## x ##_ring_ul_t
#define F(x) ceal_## x ##_ring_ul
#define E(x) CEAL_## x ##_ring_UL
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#define Tv   float
#define Fv   "%f"
#define S(x) ceal_## x ##_ring_f_s
#define T(x) ceal_## x ##_ring_f_t
#define F(x) ceal_## x ##_ring_f
#define E(x) CEAL_## x ##_ring_F
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#define Tv   double
#define Fv   "%g"
#define S(x) ceal_## x ##_ring_d_s
#define T(x) ceal_## x ##_ring_d_t
#define F(x) ceal_## x ##_ring_d
#define E(x) CEAL_## x ##_ring_D
#include "modref_functor_ring.h"
#include "modref_functor_ring.c"
#include "modref_functor_undef.h"

#endif
