/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_awar.c
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

/*
  Matthew Hammer <hammer@ttic.edu>
*/

#ifndef __CEAL_MODREF_AWAR_C__
#define __CEAL_MODREF_AWAR_C__

#include "trace.h"

#define Feedback_flag 0

#define Tv   void*
#define Fv   "%p"
#define S(x) ceal_## x ##_awar_p_s
#define T(x) ceal_## x ##_awar_p_t
#define F(x) ceal_## x ##_awar_p
#define E(x) CEAL_## x ##_awar_P
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#define Tv   int
#define Fv   "%d"
#define S(x) ceal_## x ##_awar_i_s
#define T(x) ceal_## x ##_awar_i_t
#define F(x) ceal_## x ##_awar_i
#define E(x) CEAL_## x ##_awar_I
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#define Tv   unsigned int
#define Fv   "%ud"
#define S(x) ceal_## x ##_awar_ui_s
#define T(x) ceal_## x ##_awar_ui_t
#define F(x) ceal_## x ##_awar_ui
#define E(x) CEAL_## x ##_awar_UI
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#define Tv   long
#define Fv   "%ld"
#define S(x) ceal_## x ##_awar_l_s
#define T(x) ceal_## x ##_awar_l_t
#define F(x) ceal_## x ##_awar_l
#define E(x) CEAL_## x ##_awar_L
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#define Tv   unsigned long
#define Fv   "%ld"
#define S(x) ceal_## x ##_awar_ul_s
#define T(x) ceal_## x ##_awar_ul_t
#define F(x) ceal_## x ##_awar_ul
#define E(x) CEAL_## x ##_awar_UL
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#define Tv   float
#define Fv   "%f"
#define S(x) ceal_## x ##_awar_f_s
#define T(x) ceal_## x ##_awar_f_t
#define F(x) ceal_## x ##_awar_f
#define E(x) CEAL_## x ##_awar_F
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#define Tv   double
#define Fv   "%g"
#define S(x) ceal_## x ##_awar_d_s
#define T(x) ceal_## x ##_awar_d_t
#define F(x) ceal_## x ##_awar_d
#define E(x) CEAL_## x ##_awar_D
#include "modref_functor_awar.h"
#include "modref_functor_awar.c"
#include "modref_functor_undef.h"

#endif
