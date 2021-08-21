SHELL=/bin/sh

OPTFLAGS ?= -unsafe
LIBS     ?=

NPROC    ?= $(shell nproc)
EXE      ?= drag
IMPL     ?= primes_faithful
WANT     ?= $(EXE)-st

REPEATS  ?= 10 # how many times to run for error normalization
PERFTARG ?= # you must choose only one target to run perf on, it's expensive

.PHONY: run perf clean distclean


all: run


$(EXE)-st: $(IMPL)_st.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-im: $(IMPL)_im.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-mc: $(IMPL)_mc.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-mt: $(IMPL)_mt.ml
	ocamlopt $(OPTFLAGS) -o $@ -I +threads unix.cmxa threads.cmxa $(LIBS) $<

$(EXE)-fk: $(IMPL)_fk.ml
	ocamlopt $(OPTFLAGS) -o $@ unix.cmxa $(LIBS) $<

$(EXE)-1b: $(IMPL)_1b.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-hb: $(IMPL)_hb.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-bb: $(IMPL)_bb.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-bm: $(IMPL)_bm.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

$(EXE)-bs: $(IMPL)_bs.ml
	ocamlopt $(OPTFLAGS) -o $@ $(LIBS) $<

run: $(WANT)
	@echo Running executables $(REPEATS) times with -t $(NPROC):
	@for i in $(EXE)*; do \
	  for j in `seq 1 $(REPEATS)`; do \
	    ./$$i -t $(NPROC); \
	    printf "%d, " $$j 1>&2; \
	  done |cut -d';' -f2 |paste -sd'+' \
	       |sed 's:.*:(&)/$(REPEATS):' |bc |sed 's/^/'$$i': /'; \
	done

perf: guard-PERFTARG runperf

runperf: $(PERFTARG).data
	@perf report -i $<

$(PERFTARG).data: $(PERFTARG)
	perf record -o $@ --call-graph dwarf -- ./$< -t $(NPROC)

clean:
	rm -f *.o *.cm?

distclean: clean
	rm -f $(EXE)*
	rm -f perf*


guard-%:
	@[ -n "${$*}" ]

