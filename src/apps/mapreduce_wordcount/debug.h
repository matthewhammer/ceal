/*
 * Code stolen from Dave Andersen's FAWN project at CMU
 *
 * Code stolen and modified by Reinhard Munz <munz@mpi-sws.org>
 */

#ifndef _DEBUG_H_
#define _DEBUG_H_

#ifndef DEBUG
//#define DEBUG
#endif


/*
 * The format of this should be obvious.  Please add some explanatory
 * text if you add a debugging value.
 */
#define DEBUG_NONE      0x00	// DBTEXT:  No debugging
#define DEBUG_ERRS      0x01	// DBTEXT:  Verbose error reporting
#define DEBUG_FLOW      0x02	// DBTEXT:  Messages to understand flow
#define DEBUG_CALL      0x04	// DBTEXT:  Messages to understand function calls

#define DEBUG_ALL       0xffffffff


#ifdef DEBUG

#define debug_level DEBUG_FLOW

#include <assert.h>
#include <stdio.h>  /* for perror */
#define eprintf(fmt, args...) fprintf(stderr, fmt, ##args)

#define DPRINTF(level, fmt, args...) \
        do { if (debug_level & (level)) fprintf(stderr, fmt , ##args ); } while(0)
#define DEBUG_PERROR(errmsg) \
        do { if (debug_level & DEBUG_ERRS) perror(errmsg); } while(0)
#define DASSERT(condition) \
        do { assert(condition); } while(0)
#else
#define DPRINTF(args...)
#define DEBUG_PERROR(args...)
#define DASSERT(args...)

#endif

#endif /* _DEBUG_H_ */
