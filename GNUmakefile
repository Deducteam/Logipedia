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
	@echo "[DKC] $^"
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

library: $(LIBDKS:.dk=.dko) $(LIBDKS:.dk=.summary)

library/%.dko library/%.stt library/%.tex library/%.pvs: library/%.dk .library_depend main.native
	@echo "[STT,TEX,PVS] $<"
	@./main.native -I library -I theories $<

library/%.pdf: library/%.tex
	@echo "[PDF] $@"
	@pdflatex -halt-on-error -output-directory=library $< > /dev/null || echo "ERROR on $@"

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	@proveit --importchain -sf $<

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
	@find . -name "*.summary" -exec rm {} \;
	@find . -name ".pvs_context" -exec rm {} \;
	@find . -name "*.prf" -exec rm {} \;
	@find . -name "*.bin" -exec rm {} \;
	@find . -name "*.dep" -exec rm {} \;

.PHONY: all clean distclean examples library
