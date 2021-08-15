SHELL=/bin/sh

OPTFLAGS ?=
NPROC    ?= $(shell nproc)
EXE      ?= drag
IMPL     ?= primes_faithful
WANT     ?= $(EXE)-st

REPEATS  ?= 10 # how many times to run for error normalization
PERFTARG ?= # you must choose only one target to run perf on, it's expensive

.PHONY: run perf clean $(EXE)-st $(EXE)-mt $(EXE)-im $(EXE)-mc


all: run

$(EXE)-st: $(IMPL).cmx
	ocamlopt $(OPTFLAGS) -o $@ $<

$(EXE)-im: $(IMPL)_imp.cmx
	ocamlopt $(OPTFLAGS) -o $@ $<

$(EXE)-mc: $(IMPL)_mc.cmx
	ocamlopt $(OPTFLAGS) -o $@ $<

$(EXE)-mt: $(IMPL)_mt.cmx
	ocamlopt $(OPTFLAGS) -I +threads -o $@ unix.cmxa threads.cmxa $<

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

distclean:
	rm -f $(EXE)*


%.cmx %.cma %.o: %.ml
	ocamlopt $(OPTFLAGS) -c $<

guard-%:
	@[ -n "${$*}" ]

