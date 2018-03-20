DKCHECK = dkcheck
DKDEP   = dkdep

all: main.native

#### Main program ##################################################

main.native: _build/src/main.native

_build/src/main.native: $(wildcard src/*.ml src/*.mli)
	@echo "[OPT] main.native"
	@ocamlbuild -quiet -package dedukti.parser src/main.native

#### Producing the theory file #####################################

theories/sttfa.dko: theories/sttfa.dk
	$(DKCHECK) -e $^

#### Running examples ##############################################

examples: theories/sttfa.dko main.native
	for f in $(wildcard examples/*.dk); do \
		./main.native -I theories $$f ; \
	done;

#### Producing the Dedukti library #################################

LIBDKS  = $(wildcard library/*.dk)

library: $(LIBDKS:.dk=.dko)

library/%.dko: library/%.dk .depend
	$(DKCHECK) -I library -I theories -e $<

.depend: $(wildcard library/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .depend
endif
endif

#### Cleaning targets ##############################################

clean:
	@ocamlbuild -clean -quiet
	@rm -f .depend

distclean: clean
	@find . -name "*~" -exec rm {} \;
	@find . -name "*.dko" -exec rm {} \;

.PHONY: all clean distclean examples library
