DKCHECK = dkcheck
DKDEP   = dkdep

all: main.native

#### Main program ##################################################

main.native: _build/src/main.native

_build/src/main.native: $(wildcard src/*.ml src/*.mli src/export/*.ml src/export/*.mli)
	@echo "[OPT] main.native"
	@ocamlbuild -quiet -Is src/,src/export -package dedukti.kernel -package dedukti.parser src/main.native

#### Producing the theory file #####################################

theories/sttfa.dko: theories/sttfa.dk
	@echo "[DKC sttforall] $^"
	@$(DKCHECK) -e $^

#### Running examples ##############################################

EXADKS = $(wildcard examples/*.dk)

examples: $(EXADKS:.dk=.stt) $(EXADKS:.dk=.pdf)

examples/%.dko examples/%.stt examples/%.tex: examples/%.dk theories/sttfa.dko main.native
	@echo "[STT] $<"
	@./main.native -I theories $<

examples/%.pdf: examples/%.tex
	@echo "[PDF] $@"
	@pdflatex -output-directory=examples $< > /dev/null || echo "ERROR on $@"

#### Producing the Dedukti library #################################

LIBDKS = $(wildcard library/*.dk)

library: $(LIBDKS:.dk=.dko)

library/%.v library/%.ma library/%.pvs library/%.dko:  library/%.dk theories/sttfa.dko .library_depend main.native
	@echo "[PVS,COQ] $<"
	@./main.native -I library -I theories $<

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	proveit --importchain -sf $<

SORTEDDKS = $(shell dkdep --ignore -I library -s $(LIBDKS))
SORTEDV = $(SORTEDDKS:.dk=.v)
coq: $(LIBDKS:.dk=.dko)
	coqc -R library "" $(SORTEDV)

library/%.pdf: library/%.tex
	@echo "[PDF] $@"
	@pdflatex -halt-on-error -output-directory=library $< > /dev/null || echo "ERROR on $@"

.PRECIOUS: library/%.pvs

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	proveit --importchain -sf $<

.library_depend: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend
endif
endif

#### Cleaning targets ##############################################

clean:
	@ocamlbuild -clean -quiet
	@rm -f .library_depend

distclean: clean
	@find . -name "*~" -exec rm {} \;
	@find . -name "*.dko" -exec rm {} \;
	@find . -name "*.stt" -exec rm {} \;
	@find . -name "*.aux" -exec rm {} \;
	@find . -name "*.log" -exec rm {} \;
	@find . -name "*.pdf" -exec rm {} \;
	@find . -name "*.tex" -exec rm {} \;
	@find . -name "*.pvs" -exec rm {} \;
	@find . -name "*.prf" -exec rm {} \;
	@find . -name "*.bin" -exec rm {} \;
	@find . -name "*.dep" -exec rm {} \;
	@find . -name "*.ma"  -exec rm {} \;
	@find . -name "*.v"   -exec rm {} \;
	@find . -name "*.vo"  -exec rm {} \;
	@find . -name "*.summary" -exec rm {} \;
	@find . -name ".pvs_context" -exec rm {} \;

.PHONY: all clean distclean examples library coq
