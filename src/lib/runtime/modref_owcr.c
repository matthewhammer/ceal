/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_owcr.c
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

#ifndef __CEAL_MODREF_OWCR_C__
#define __CEAL_MODREF_OWCR_C__

#include "trace.h"

#define Tv   void*
#define Fv   "%p"
#define S(x) ceal_## x ##_owcr_p_s
#define T(x) ceal_## x ##_owcr_p_t
#define F(x) ceal_## x ##_owcr_p
#define E(x) CEAL_## x ##_owcr_P
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#define Tv   int
#define Fv   "%d"
#define S(x) ceal_## x ##_owcr_i_s
#define T(x) ceal_## x ##_owcr_i_t
#define F(x) ceal_## x ##_owcr_i
#define E(x) CEAL_## x ##_owcr_I
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#define Tv   unsigned int
#define Fv   "%ud"
#define S(x) ceal_## x ##_owcr_ui_s
#define T(x) ceal_## x ##_owcr_ui_t
#define F(x) ceal_## x ##_owcr_ui
#define E(x) CEAL_## x ##_owcr_UI
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#define Tv   long
#define Fv   "%ld"
#define S(x) ceal_## x ##_owcr_l_s
#define T(x) ceal_## x ##_owcr_l_t
#define F(x) ceal_## x ##_owcr_l
#define E(x) CEAL_## x ##_owcr_L
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#define Tv   unsigned long
#define Fv   "%ld"
#define S(x) ceal_## x ##_owcr_ul_s
#define T(x) ceal_## x ##_owcr_ul_t
#define F(x) ceal_## x ##_owcr_ul
#define E(x) CEAL_## x ##_owcr_UL
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#define Tv   float
#define Fv   "%f"
#define S(x) ceal_## x ##_owcr_f_s
#define T(x) ceal_## x ##_owcr_f_t
#define F(x) ceal_## x ##_owcr_f
#define E(x) CEAL_## x ##_owcr_F
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#define Tv   double
#define Fv   "%g"
#define S(x) ceal_## x ##_owcr_d_s
#define T(x) ceal_## x ##_owcr_d_t
#define F(x) ceal_## x ##_owcr_d
#define E(x) CEAL_## x ##_owcr_D
#include "modref_functor_owcr.h"
#include "modref_functor_owcr.c"
#include "modref_functor_undef.h"

#endif
