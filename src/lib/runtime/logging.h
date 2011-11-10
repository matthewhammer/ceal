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
/* Facilities for logging */

#ifndef __CEAL_LOGGING_H__
#define __CEAL_LOGGING_H__
#include <stdio.h>

extern FILE* ceal_logg_file;

void ceal_logg_init(void);

#if CEAL_LOGGING
#define logg(FMT, ...) \
  fprintf(ceal_logg_file, "%-25s : " FMT "\n",__FUNCTION__,##__VA_ARGS__)
#else
#define logg(...)
#endif
#endif

