DKCHECK = dkcheck
DKDEP   = dkdep

all: main.native

#### Main program ##################################################

main.native: _build/src/main.native

_build/src/main.native: $(wildcard src/*.ml src/*.mli)
	ocamlbuild -package dedukti.parser src/main.native

#### Producing the theory file #####################################

sttfa.dko: theoreis/sttfa.dk
	$(DKCHECK) -e $^

#### Running examples ##############################################

examples: sttfa.dko main.native
	for f in $(wildcard examples/*.dk); do \
		./main.native $$f ; \
	done;

#### Producing the Dedukti library #################################

%.dko: library/%.dk
	$(DKCHECK) $<

.depend: $(wildcard library/*.dk)
	@$(DKDEP) --ignore $^ -o $@

#### Cleaning targets ##############################################

clean:
	ocamlbuild -clean

distclean: clean
	find . -name "*~" -exec rm {} \;

.PHONY: all clean distclean examples library
