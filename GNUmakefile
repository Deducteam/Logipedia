DKCHECK = dkcheck
DKDEP   = dkdep
MATITAC = matitac
LEAN    = lean

LOGIPEDIA = _build/default/src/main.exe

logipedia: bin
	@echo "[BUILD EXECUTABLE] logipedia"
	@rm logipedia 2>/dev/null || true
	@ln -s _build/default/src/main.exe logipedia

.PHONY: all
all: $(LOGIPEDIA)

$(LOGIPEDIA):
	@dune build

.PHONY: doc
doc:
	@dune build @doc

#### Producing the theory file #####################################

theories/sttfa.dko: theories/sttfa.dk
	@echo "[CHECK] $^"
	@$(DKCHECK) -e $^

theories/hol.dko: theories/hol.dk
	@echo "[CHECK] $<"
	@$(DKCHECK) -I theories -e $<

theories/hol_axioms.dko: theories/hol_axioms.dk theories/hol.dko
	@echo "[CHECK] $<"
	@$(DKCHECK) -I theories -e $<

theories/hol_sttfa.dko: theories/hol_sttfa.dk theories/sttfa.dko theories/hol.dko theories/hol_axioms.dko
	@echo "[CHECK] $<"
	@$(DKCHECK) -I theories -e $<

#### Producing the Dedukti library #################################

PACKAGE = arith_fermat
LOGIC = sttfa
IPATH = import/dedukti/$(LOGIC)/$(PACKAGE)
DKS = $(wildcard $(IPATH)/*.dk)
SORTEDDKS = $(shell dkdep -s --ignore -I $(IPATH) $(IPATH)/*.dk)



#### Dependencies ##################################################

.library_depend_dko: $(wildcard $(IPATH)/*.dk theories/$(LOGIC).dk)
	@echo "[DKDEP (DK FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^

.library_depend_v: $(wildcard $(IPATH)/*.dk theories/$(LOGIC).dk)
	@echo "[DKDEP (V FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/v/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.v:$(COQPATH)/\1\.v:g" $@

.library_depend_ma: $(wildcard $(IPATH)/*.dk theories/$(LOGIC).dk)
	@echo "[DKDEP (MA FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/ma/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.ma:$(MATITAPATH)/\1\.ma:g" $@

.library_depend_lean: $(wildcard $(IPATH)/*.dk theories/$(LOGIC).dk)
	@echo "[DKDEP (LEAN FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/lean/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.lean:$(LEANPATH)/\1\.lean:g" $@

#### Export ########################################################

#### Dedukti #######################################################

$(IPATH)/%.dko: $(IPATH)/%.dk theories/$(LOGIC).dko
	@echo "[CHECK] $@"
	@$(DKCHECK) -e -I theories -I $(IPATH) $<

#### Coq ###########################################################

COQPATH = export/coq
VFILES = $(addprefix $(COQPATH)/, $(addsuffix .v, $(basename $(notdir $(wildcard $(IPATH)/*.dk)))))

$(COQPATH)/%.v: $(IPATH)/%.dko theories/$(LOGIC).dko .library_depend_v $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) -I $(IPATH) -I theories --export coq $(<:.dko=.dk) -o $@

$(COQPATH)/_CoqProject: $(VFILES)
	@cd $(COQPATH) && ls *.v > _CoqProject

$(COQPATH)/Makefile: $(COQPATH)/_CoqProject
	@cd $(COQPATH) && coq_makefile -f _CoqProject -o Makefile

coq: $(COQPATH)/Makefile $(VFILES)
	@cd $(COQPATH) && make
	@echo "[COQ] CHECKED"


#### Matita ########################################################

MATITAPATH = export/matita
MAFILES=$(addprefix $(MATITAPATH)/, $(addsuffix .ma, $(basename $(notdir $(wildcard $(IPATH)/*.dk)))))

$(MATITAPATH)/%.ma: $(IPATH)/%.dko theories/$(LOGIC).dko .library_depend_ma $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) -I $(IPATH) -I theories --export matita $(<:.dko=.dk) -o $@

$(MATITAPATH)/root:
	@echo "baseuri = cic:/matita" > $@

matita: $(MAFILES) $(MATITAPATH)/root
	@cd $(MATITAPATH) && $(MATITAC) *.ma
	@echo "[MATITA] CHECKED"

#### Lean ##########################################################

LEANPATH = export/lean
LEANFILES=$(addprefix $(LEANPATH)/,$(addsuffix .lean,$(basename $(notdir $(wildcard $(IPATH)/*.dk)))))

$(LEANPATH)/%.lean: $(IPATH)/%.dko theories/$(LOGIC).dko .library_depend_lean $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) -I $(IPATH) -I theories --export lean $(<:.dko=.dk) -o $@

lean: $(LEANFILES)
	@cd $(LEANPATH) && $(LEAN) *.lean
	@echo "[LEAN] CHECKED"

# library/%.lean:  library/%.dk theories/sttfa.dko .library_depend_lean $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --export lean $(BDD) $<

# library/%.pvs: library/%.dk theories/sttfa.dko .library_depend_pvs $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --export pvs $(BDD) $<

# library/%.ma: library/%.dk theories/sttfa.dko .library_depend_ma $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --export matita $(BDD) $<

# library/%.art: library/%.dk theories/sttfa.dko .library_depend_art $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --export opentheory $(BDD) $<

# library/%.thy: .library_depend_dko
# 	@echo "[GENERATE] $@"
# 	@python3 bin/gen-thy-file.py $(notdir $(basename $@)) > $@

# library/%.summary: library/%.pvs
# 	@echo "[SUMMARY]"
# 	proveit --importchain -sf $<

# .PRECIOUS: library/%.pvs

# library/%.summary: library/%.pvs
# 	@echo "[SUMMARY]"
# 	proveit --importchain -sf $<

# library/open_theory/%.art: library/open_theory/%.dk theories/hol.dko .library_depend_art $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --theory hol --export opentheory $(BDD) $<

# library/open_theory/%.v: library/open_theory/%.dk theories/hol.dko .library_depend_art $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --theory hol --export coq $(BDD) $<

# library/open_theory/packages/%.art: library/open_theory/packages/%.dk theories/hol.dko .library_depend_art $(MAIN)
# 	@echo "[EXPORT] $@"
# 	@./main.native -I library -I theories --theory hol --export opentheory $(BDD) $<

# web: theories/sttfa.dko theories/hol.dko $(MAIN)
# 	mongo < ./bdd/dropLogipedia.js
# 	time ./main.native  -I library -I theories --export-web $(SORTEDDKS)

# matita: library/fermat.ma
# 	$(MATITAC) library/fermat.ma | grep -v 'refinement' | grep -v 'ELPI' | grep -v 'type-checking'

# pvs: library/fermat.pvs
# 	proveit --importchain -sf library/fermat.pvs

# lean: library/fermat.lean
# 	leanpkg new /tmp/fermat
# 	cp library/*.lean /tmp/fermat/src/
# 	lean /tmp/fermat/src/fermat.lean

# opentheory: library/fermat.thy library/fermat.art
# 	opentheory info library/fermat.thy

# .library_depend_pvs: $(wildcard library/*.dk theories/*.dk examples/*.dk)
# 	@echo "[DEP] $@"
# 	@$(DKDEP) -o $@ -I library -I theories $^
# 	@sed -i s/theories\\/sttfa.dko//g $@
# 	@sed -i s/dko/pvs/g $@

# .library_depend_ma: $(wildcard library/*.dk theories/*.dk examples/*.dk)
# 	@echo "[DEP] $@"
# 	@$(DKDEP) -o $@ -I library -I theories $^
# 	@sed -i s/theories\\/sttfa.dko//g $@
# 	@sed -i s/dko/ma/g $@

# .library_depend_art: $(wildcard library/*.dk theories/*.dk examples/*.dk)
# 	@echo "[DEP] $@"
# 	@$(DKDEP) -o $@ -I library -I theories $^
# 	@sed -i s/theories\\/sttfa.dko//g $@
# 	@sed -i s/theories\\/hol.dko//g $@
# 	@sed -i s/dko/art/g $@

# .library_depend_lean: $(wildcard library/*.dk theories/*.dk examples/*.dk)
# 	@echo "[DEP] $@"
# 	@$(DKDEP) -o $@ -I library -I theories $^
# 	@sed -i s/theories\\/sttfa.dko//g $@
# 	@sed -i s/dko/lean/g $@



#### Dependencies ##################################################

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
-include .library_depend_v
-include .library_depend_ma
-include .library_depend_lean
endif
endif

#### Cleaning targets ##############################################

.PHONY: clean
clean:
	@dune clean
	@rm -f .library_depend_dko
	@rm -f .library_depend_v
	@rm -f .library_depend_vo
	@rm -f .library_depend_ma
	@rm -f .library_depend_lean
	@rm -f .library_depend_pvs
	@rm -f .library_depend_art

.PHONY: distclean
distclean: clean
	@find . -name "\#*"          -exec rm {} \;
	@find . -name "*~"           -exec rm {} \;
	@find . -name "*.dko"        -exec rm {} \;
	@find . -name "*.stt"        -exec rm {} \;
	@find . -name "*.aux"        -exec rm {} \;
	@find . -name "*.log"        -exec rm {} \;
	@find . -name "*.pdf"        -exec rm {} \;
	@find . -name "*.tex"        -exec rm {} \;
	@find . -name "*.pvs"        -exec rm {} \;
	@find . -name "*.prf"        -exec rm {} \;
	@find . -name "*.bin"        -exec rm {} \;
	@find . -name "*.dep"        -exec rm {} \;
	@find . -name "*.ma"         -exec rm {} \;
	@find . -name "*.v"          -exec rm {} \;
	@find . -name "*.vo"         -exec rm {} \;
	@find . -name "*.glob"       -exec rm {} \;
	@find . -name "*.lean"       -exec rm {} \;
	@find . -name "*.json"       -exec rm {} \;
	@find . -name "*.art"        -exec rm {} \;
	@find . -name "*.thy"        -exec rm {} \;
	@find . -name "*.summary"    -exec rm {} \;
	@find . -name "*.beautified" -exec rm {} \;
	@find . -name ".pvscontext"  -exec rm {} \;
	@rm -rf /tmp/fermat
