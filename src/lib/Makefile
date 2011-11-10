## Input
SRCDIR=.
VPATH=\
	$(SRCDIR)/runtime\
	$(SRCDIR)/testperf\

INCLUDE=-I$(SRCDIR)/runtime\
	-I$(SRCDIR)/testperf

LIBCEAL_HEADERS=\
	libceal.h\
	\
	logging.h\
	tv_signal.h\
	freelist.h\
	freeset.h\
	basemm.h\
	totalorder.h\
	trace.h\
	state.h\
	stack.h\
	pqueue.h\
	hashtbl.h\
	hash_util32.h\
	hash_util64.h\
	hash.h\
	modref_awar.h\
	modref_owcr.h\
	modref_ring.h\
	\
	test_params.h\
	test_time.h\
	test_utils.h\
	test_types.h\

LIBCEAL_HEADERS_DEP=\
	$(LIBCEAL_HEADERS) \
	modref_functor_awar.h\
	modref_functor_awar.c\
	modref_functor_zwzr.h\
	modref_functor_zwzr.c\
	modref_functor_owcr.h\
	modref_functor_owcr.c\
	modref_functor_ring.h\
	modref_functor_ring.c\
	modref_functor_undef.h\

LIBCEAL_SOURCES=\
	logging.c\
	tv_signal.c\
	freelist.c\
	freeset.c\
	basemm.c\
	totalorder.c\
	stack.c\
	pqueue.c\
	hashtbl.c\
	hash.c\
	trace.c\
	modref_awar.c\
	modref_zwzr.c\
	modref_owcr.c\
	modref_ring.c\
	random.c\
	\
	test_params.c\
	test_time.c\
	test_utils.c\
	test_main.c\
	test_power.c\

CFLAGS += -g 
#CFLAGS += -pg
CFLAGS += -O0
#CFLAGS += -O3
CFLAGS += -Wall

CEAL_FLAGS += -DCEAL_HASHTBL_INIT_SIZE=64
CEAL_FLAGS += -DCEAL_HASHTBL_MAXCAP_BITS=1
CEAL_FLAGS += -DCEAL_HASHTBL_MINCAP_BITS=3
CEAL_FLAGS += -DCEAL_HASHTBL_SHRINK_BITS=3
CEAL_FLAGS += -DCEAL_HASHTBL_GROWTH_BITS=3
CEAL_FLAGS += -DCEAL_MEMO_PREFERS_EARLIEST_MATCH=1
CEAL_FLAGS += -DCEAL_BASEMM_FREESET_MAXBLOCK=1024
CEAL_FLAGS += -DCEAL_BASEMM_STATS
CEAL_FLAGS += -DCEAL_ANALYTIC_STATS
#CEAL_FLAGS += -DCEAL_BASEMM_FREESET=1
CEAL_FLAGS += -DCEAL_BASEMM_FREESET=0
CEAL_FLAGS += -DCEAL_DEBUG_MODTYP
CEAL_FLAGS += -DCEAL_DEBUG
#CEAL_FLAGS += -DCEAL_LOGGING
#CEAL_FLAGS += -DCEAL_LOGGING_STDERR

## Tools
CC=gcc
CPP=gcc -E -P
CPPTOOL ?= $(SRCDIR)/scripts/cpp-tool.pl

## Output
BINDIR ?=../../bin/lib
LIBCEAL_OBJECTS=$(addprefix $(BINDIR)/, $(LIBCEAL_SOURCES:.c=.o))
LIBCEAL=$(BINDIR)/libceal.a
LIBCEALOPT=$(BINDIR)/libceal.o
LIBCEALH=$(BINDIR)/libceal.h

ALLSOURCEDEP=$(LIBCEAL_HEADERS) $(LIBCEAL_HEADERS_DEP) $(LIBCEAL_SOURCES)
ALLSOURCEDUM=$(BINDIR)/dummy.c

.Phony: default clean lines 

default: $(LIBCEALH) $(LIBCEAL) $(LIBCEALOPT) 

# I got sick of missing dependencies from bugs in this Makefile.  They
# were mostly due to my wierd use of #includes to do functor-like
# things.  This file depends on everything that could concievably
# affect the runtime.  In turn, the runtime libraries we produce each
# depend on it.  Since we are doing a "whole-program" style compilation, and since the runtime still compiles fast enough (under a minute)
$(ALLSOURCEDUM): Makefile $(ALLSOURCEDEP) | $(BINDIR)
	@echo "Creating $@."
	@echo "/* Conservatively, everything depends on: */" > $@
	@echo "/* $^ */" >> $@

clean:
	-rm -rf $(BINDIR)

lines: $(ALLSOURCEDEP)
	wc -l $^

$(BINDIR):
	mkdir -p $(BINDIR)

$(BINDIR)/%.o: %.c $(ALLSOURCEDUM) | $(BINDIR)
	@printf "Compiling %-32s into %-32s\n" $< $@
	@$(CC) $(CFLAGS) $(CEAL_FLAGS) $(INCLUDE) -c $< -o $@


$(LIBCEAL): $(ALLSOURCEDUM) $(LIBCEAL_OBJECTS)
#	gcc -shared -Wl,-soname,$(LIBCEAL_SONAME) -o $@ $(LIBCEAL_OBJECTS) -lc
	@echo Creating $@
	@ar rcs $@ $(LIBCEAL_OBJECTS)

$(LIBCEALOPT): $(ALLSOURCEDUM) $(LIBCEAL_HEADER) $(LIBCEAL_SOURCES)
	@printf "Compiling %-32s into %-32s (using gcc -combine)\n" "all C sources" $@
	@gcc -combine $(CFLAGS) $(CEAL_FLAGS) $(INCLUDE) $(filter %.c,$^) -c -o $@

$(LIBCEALH): libceal.h $(ALLSOURCEDUM) $(LIBCEAL_HEADERS) | $(BINDIR)
	@echo "Creating $@"
	@$(CPP) $(CEAL_FLAGS) $< -o $@

# -- Save a copy of runtime.h --
# Since $(CFLAGS) can affect the declarations in runtime.h, we must
# ensure that when the cealc compiler reads runtime.h, it sees a
# version that is consistent with the libceal.a library.
# $(LIBCEAL_HEADER): runtime.h $(ALLSOURCEDUM) $(LIBCEAL_HEADERS) | $(BINDIR)
#	@echo Creating $(patsubst %.i,%.h,$@), $@ and $(patsubst %.i,%.dM.h,$@)
#	$(CPP) $< -o $(patsubst %.i,%.h,$@)
#	$(CPP) $(CEAL_FLAGS) $<     -o $@
#	$(CPP) $(CEAL_FLAGS) $< -dM -o $(patsubst %.i,%.dM.h,$@)
