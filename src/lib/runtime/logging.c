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

#include <stdio.h>
#include "logging.h"

FILE* ceal_logg_file = NULL;

void ceal_logg_init() {

#if CEAL_LOGGING

#if CEAL_LOGGING_STDERR
  ceal_logg_file = stderr;

#else
  if(!ceal_logg_file) {
    const char* logg_filename = "ceal.log";
    ceal_logg_file = fopen(logg_filename,"w+");

    logg   ("opened logg file for writing: %s", logg_filename);

    fprintf(stderr,
            "opened logg file for writing: %s\n", logg_filename);
  }

#if 1
  /* Don't do any buffering of the log. */
  setvbuf(ceal_logg_file, NULL, _IONBF, 0);
#else
  /* Do line buffering. */
  setvbuf(ceal_logg_file, NULL, _IOLBUF, 0);
#endif
  
#endif
#endif
}
