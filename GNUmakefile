DKCHECK = dkcheck
DKDEP   = dkdep
MATITAC = matitac

MAIN = _build/src/main.native

all: $(MAIN)

#### Main program ##################################################

$(MAIN): $(wildcard src/*.ml src/*.mli src/export/*.ml src/export/*.mli src/utils/*.ml src/utils/*.mli)
	@echo "[BUILD] main.native"
	@ocamlbuild -use-ocamlfind -quiet -Is src/,src/utils,src/export -tag thread -package mongo -package dedukti.kernel -package dedukti.parser src/main.native

#### Producing the theory file #####################################

theories/sttfa.dko: theories/sttfa.dk
	@echo "[CHECK] $^"
	@$(DKCHECK) -e $^

#### Running examples ##############################################

EXADKS = $(wildcard examples/*.dk)

examples: $(EXADKS:.dk=.stt) $(EXADKS:.dk=.pdf)

examples/%.dko examples/%.stt examples/%.tex: examples/%.dk theories/sttfa.dko $(MAIN)
	@echo "[STT] $<"
	@./main.native -I theories $<

examples/%.pdf: examples/%.tex
	@echo "[PDF] $@"
	@pdflatex -output-directory=examples $< > /dev/null || echo "ERROR on $@"

#### Producing the Dedukti library #################################

LIBDKS = $(wildcard library/*.dk)
SORTEDDKS = $(shell dkdep -s -I theories/ -I library/ --ignore library/*.dk | cut -d" " -f 2,2-)

library/%.lean:  library/%.dk theories/sttfa.dko .library_depend_lean $(MAIN)
	@echo "[EXPORT] $@"
	@./main.native -I library -I theories --export lean $(BDD) $<

library/%.pvs: library/%.dk theories/sttfa.dko .library_depend_pvs $(MAIN)
	@echo "[EXPORT] $@"
	@./main.native -I library -I theories --export pvs $(BDD) $<

library/%.v: library/%.dk theories/sttfa.dko .library_depend_v $(MAIN)
	@echo "[EXPORT] $@"
	@./main.native -I library -I theories --export coq $(BDD) $<
	@coqc -R library "" -beautify $@
	@mv $@.beautified $@

library/%.ma: library/%.dk theories/sttfa.dko .library_depend_ma $(MAIN)
	@echo "[EXPORT] $@"
	@./main.native -I library -I theories --export matita $(BDD) $<

library/%.art: library/%.dk theories/sttfa.dko .library_depend_art $(MAIN)
	@echo "[EXPORT] $@"
	@./main.native -I library -I theories --export opentheory $(BDD) $<

library/%.thy: library/%.art
	@echo "[GENERATE] $@"
	@python3 bin/gen-thy-file.py $(notdir $(basename $@)) > $@

library/%.vo: library/%.v .library_depend_vo
	@echo "[CHECK] $@"
	@coqc -R library "" $<

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	proveit --importchain -sf $<

.PRECIOUS: library/%.pvs

library/%.summary: library/%.pvs
	@echo "[SUMMARY]"
	proveit --importchain -sf $<

coq: library/fermat.vo

matita: library/fermat.ma
	$(MATITAC) library/fermat.ma | grep -v 'refinement' | grep -v 'ELPI' | grep -v 'type-checking'

pvs: library/fermat.pvs
	proveit --importchain -sf library/fermat.pvs

lean: library/fermat.lean
	leanpkg new /tmp/fermat
	cp library/*.lean /tmp/fermat/src/
	lean /tmp/fermat/src/fermat.lean

opentheory: library/fermat.thy
	opentheory info library/fermat.thy

bdd: $(LIBDKS) main.native theories/sttfa.dko
	for i in $(SORTEDDKS) ; do \
		./main.native  -I library -I theories $$i ; \
	done
.library_depend_dko: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^

.library_depend_pvs: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/pvs/g $@

.library_depend_v: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/v/g $@

.library_depend_vo: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/vo/g $@
	@sed -i s/dk/v/g $@

.library_depend_ma: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/ma/g $@

.library_depend_art: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/art/g $@

.library_depend_lean: $(wildcard library/*.dk theories/*.dk examples/*.dk)
	@echo "[DEP] $@"
	@$(DKDEP) -o $@ -I library -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/lean/g $@

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
-include .library_depend_pvs
-include .library_depend_vo
-include .library_depend_v
-include .library_depend_ma
-include .library_depend_art
-include .library_depend_lean
endif
endif

#### Cleaning targets ##############################################

clean:
	@ocamlbuild -clean -quiet
	@rm -f .library_depend_dko
	@rm -f .library_depend_v
	@rm -f .library_depend_vo
	@rm -f .library_depend_ma
	@rm -f .library_depend_lean
	@rm -f .library_depend_pvs
	@rm -f .library_depend_art

distclean: clean
	@find library -name "\#*"    -exec rm {} \;
	@find . -name "*~"           -exec rm {} \;
	@find . -name "*.dko"        -exec rm {} \;
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
	@find library -name "*.json" -exec rm {} \;
	@find library -name "*.art"  -exec rm {} \;
	@find library -name "*.summary" -exec rm {} \;
	@find library -name "*.beautified" -exec rm {} \;
	@find library -name ".pvs_context" -exec rm {} \;
	@rm -rf /tmp/fermat

.PHONY: all clean distclean examples library coq matita pvs
