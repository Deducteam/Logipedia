DKCHECK = dkcheck
DKDEP   = dkdep
MATITAC = matitac

all: main.native

#### Main program ##################################################

main.native: _build/src/main.native

_build/src/main.native: $(wildcard src/*.ml src/*.mli src/export/*.ml src/export/*.mli)
	@echo "[BUILD] main.native"
	@ocamlbuild -quiet -Is src/,src/utils,src/export -package dedukti.kernel -package dedukti.parser src/main.native

#### Producing the theory file #####################################

theories/sttfa.dko: theories/sttfa.dk
	@echo "[CHECK] $^"
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

library/%.lean library/%.pvs library/%.art library/%.dko:  library/%.dk theories/sttfa.dko .library_depend_dko main.native
	@echo "[EXPORT] $@"
	@./main.native -I library -I theories $<

library/%.v: library/%.dk theories/sttfa.dko .library_depend_v main.native
	@echo "[EXPOR] $@"
	@./main.native -I library -I theories --export coq $<

library/%.ma: library/%.dk theories/sttfa.dko .library_depend_ma main.native
	@echo "[EXPOR] $@"
	@./main.native -I library -I theories --export matita $<

library/%.vo: library/%.v .library_depend_vo
	@echo "[CHECK] $@"
	@coqc -R library "" $<

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	proveit --importchain -sf $<

pvs: $(LIBDKS:.dk=.dko)
	proveit --importchain -sf library/fermat.pvs

SORTEDDKS = $(shell dkdep --ignore -I library -s $(LIBDKS))
SORTEDV = $(SORTEDDKS:.dk=.v)

coq: library/fermat.vo

matita: library/fermat.ma
	$(MATITAC) library/fermat.ma | grep -v 'refinement' | grep -v 'ELPI' | grep -v 'type-checking'

library/%.pdf: library/%.tex
	@echo "[PDF] $@"
	@pdflatex -halt-on-error -output-directory=library $< > /dev/null || echo "ERROR on $@"

.PRECIOUS: library/%.pvs

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	proveit --importchain -sf $<

.library_depend_dko: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP DKO] $@"
	@$(DKDEP) -o $@ -I library -I theories $^

.library_depend_v: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP VO] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/v/g $@

.library_depend_vo: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP VO] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/vo/g $@
	@sed -i s/dk/v/g $@

.library_depend_ma: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP VO] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/ma/g $@

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
-include .library_depend_vo
-include .library_depend_v
-include .library_depend_ma
endif
endif

#### Cleaning targets ##############################################

clean:
	@ocamlbuild -clean -quiet
	@rm -f .library_depend_dko
	@rm -f .library_depend_v
	@rm -f .library_depend_vo
	@rm -f .library_depend_ma

distclean: clean
	@find library -name "*~"     -exec rm {} \;
	@find library -name "*.dko"  -exec rm {} \;
	@find library -name "*.stt"  -exec rm {} \;
	@find library -name "*.aux"  -exec rm {} \;
	@find library -name "*.log"  -exec rm {} \;
	@find library -name "*.pdf"  -exec rm {} \;
	@find library -name "*.tex"  -exec rm {} \;
	@find library -name "*.pvs"  -exec rm {} \;
	@find library -name "*.prf"  -exec rm {} \;
	@find library -name "*.bin"  -exec rm {} \;
	@find library -name "*.dep"  -exec rm {} \;
	@find library -name "*.ma"   -exec rm {} \;
	@find library -name "*.v"    -exec rm {} \;
	@find library -name "*.vo"   -exec rm {} \;
	@find library -name "*.glob" -exec rm {} \;
	@find library -name "*.lean" -exec rm {} \;
	@find library -name "*.art"  -exec rm {} \;
	@find library -name "*.summary" -exec rm {} \;
	@find library -name "*.beautified" -exec rm {} \;
	@find library -name ".pvs_context" -exec rm {} \;

.PHONY: all clean distclean examples library coq
