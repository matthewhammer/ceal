/*
Copyright 2008-2011
CEAL Project -- Umut A. Acar and Matthew A. Hammer

File: modref_zwzr.h
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
  Matthew Hammer <hammer@mpi-sws.org>
*/

#ifndef __CEAL_MODREF_ZWZR_H__
#define __CEAL_MODREF_ZWZR_H__

#include "trace.h"

#define Tv   void*
#define S(x) ceal_## x ##_zwzr_p_s
#define T(x) ceal_## x ##_zwzr_p_t
#define F(x) ceal_## x ##_zwzr_p
#define E(x) CEAL_## x ##_zwzr_P
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#define Tv   int
#define S(x) ceal_## x ##_zwzr_i_s
#define T(x) ceal_## x ##_zwzr_i_t
#define F(x) ceal_## x ##_zwzr_i
#define E(x) CEAL_## x ##_zwzr_I
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#define Tv   unsigned int
#define Fv   "%ud"
#define S(x) ceal_## x ##_zwzr_ui_s
#define T(x) ceal_## x ##_zwzr_ui_t
#define F(x) ceal_## x ##_zwzr_ui
#define E(x) CEAL_## x ##_zwzr_UI
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#define Tv   long
#define S(x) ceal_## x ##_zwzr_l_s
#define T(x) ceal_## x ##_zwzr_l_t
#define F(x) ceal_## x ##_zwzr_l
#define E(x) CEAL_## x ##_zwzr_L
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#define Tv   unsigned long
#define Fv   "%uld"
#define S(x) ceal_## x ##_zwzr_ul_s
#define T(x) ceal_## x ##_zwzr_ul_t
#define F(x) ceal_## x ##_zwzr_ul
#define E(x) CEAL_## x ##_zwzr_UL
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#define Tv   float
#define S(x) ceal_## x ##_zwzr_f_s
#define T(x) ceal_## x ##_zwzr_f_t
#define F(x) ceal_## x ##_zwzr_f
#define E(x) CEAL_## x ##_zwzr_F
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#define Tv   double
#define S(x) ceal_## x ##_zwzr_d_s
#define T(x) ceal_## x ##_zwzr_d_t
#define F(x) ceal_## x ##_zwzr_d
#define E(x) CEAL_## x ##_zwzr_D
#include "modref_functor_zwzr.h"
#include "modref_functor_undef.h"

#endif
