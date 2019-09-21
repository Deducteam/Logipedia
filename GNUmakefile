DKCHECK = dkcheck
DKDEP   = dkdep
MATITAC = matitac
LEAN    = lean
PYTHON  = python3
OT      = opentheory
PROVEIT = proveit

LOGIPEDIA = _build/install/default/bin/logipedia

.PHONY: all
all: bin

logipedia: bin
	@echo "[BUILD EXECUTABLE] logipedia"
	-$(RM) logipedia
	@ln -s $(LOGIPEDIA) logipedia

bin: $(LOGIPEDIA)

.PHONY: $(LOGIPEDIA)
$(LOGIPEDIA):
	@dune build


.PHONY: doc
doc:
	@dune build @doc

#### Producing the theory file #####################################

theories/hol_sttfa.dko: theories/hol_sttfa.dk theories/sttfa.dko \
theories/hol.dko theories/hol_axioms.dko
	@echo "[CHECK] $<"
	@$(DKCHECK) -I theories -e $<

theories/hol_axioms.dko: theories/hol_axioms.dk theories/hol.dko
	@echo "[CHECK] $<"
	@$(DKCHECK) -I theories -e $<

theories/%.dko: theories/%.dk
	@echo "[CHECK] $^"
	@$(DKCHECK) -e -I theories/ $^


#### Producing the Dedukti library #################################

THDIR = theories
PACKAGE = arith_fermat
THEORY = sttfa
DKIMP = import/dedukti
IPATH = $(DKIMP)/$(THEORY)/$(PACKAGE)

LOGIPEDIAOPTS = -I $(IPATH) -I theories


#### Export ########################################################

#### Dedukti #######################################################

# We untar the archive here to have the list of files available
# at first run of Make
_dks := $(shell cd $(DKIMP) && \
if [ ! -d $(THEORY) ]; then \
tar xjf $(THEORY).tar.bz2; fi &&\
cd $(THEORY)/$(PACKAGE) && ls *.dk)
DKS = $(addprefix $(IPATH)/, $(_dks))
DKOS = $(patsubst %.dk,%.dko,$(DKS))
IMP = $(notdir $(basename $(DKS)))

$(IPATH)/%.dko: $(IPATH)/%.dk theories/$(THEORY).dko
	@echo "[CHECK] $@"
	@$(DKCHECK) -e -I theories -I $(IPATH) $<

.PHONY: dedukti
dedukti: theories/$(THEORY).dko $(DKOS)
	@$(DKCHECK) -I theories -I $(IPATH) $(IPATH)/fermat.dk
	@echo "[DEDUKTI] CHECKED"

#### Coq ###########################################################

COQPATH = export/coq
VFILES = $(addprefix $(COQPATH)/, $(addsuffix .v, $(IMP)))

$(COQPATH)/%.v: $(IPATH)/%.dko theories/$(THEORY).dko .library_depend_v \
$(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) $(LOGIPEDIAOPTS) --fast --export coq $(<:.dko=.dk) -o $@
	@mv $@ $(addsuffix .v, $(subst -,_, $(subst .,_,$(basename $@)))) \
|| true 2>/dev/null # avoid fail if there is no change

$(COQPATH)/_CoqProject: $(VFILES)
	@cd $(COQPATH) && ls *.v > _CoqProject

$(COQPATH)/Makefile: $(COQPATH)/_CoqProject
	@cd $(COQPATH) && coq_makefile -f _CoqProject -o Makefile

.PHONY: coq
coq: $(COQPATH)/Makefile $(VFILES)
	@cd $(COQPATH) && make
	@echo "[COQ] CHECKED"


#### Matita ########################################################

MATITAPATH = export/matita
MAFILES=$(addprefix $(MATITAPATH)/, $(addsuffix .ma, $(IMP)))

$(MATITAPATH)/%.ma: $(IPATH)/%.dko theories/$(THEORY).dko .library_depend_ma $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) $(LOGIPEDIAOPTS) --export matita $(<:.dko=.dk) -o $@

$(MATITAPATH)/root:
	@echo "baseuri = cic:/matita" > $@

.PHONY: matita
matita: $(MAFILES) $(MATITAPATH)/root
	@cd $(MATITAPATH) && $(MATITAC) *.ma
	@echo "[MATITA] CHECKED"

#### Lean ##########################################################

LEANPATH = export/lean
LEANFILES=$(addprefix $(LEANPATH)/,$(addsuffix .lean,$(IMP)))

$(LEANPATH)/%.lean: $(IPATH)/%.dko theories/$(THEORY).dko .library_depend_lean $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) $(LOGIPEDIAOPTS) --export lean $(<:.dko=.dk) -o $@

.PHONY: lean
lean: $(LEANFILES)
	@cd $(LEANPATH) && $(LEAN) *.lean
	@echo "[LEAN] CHECKED"

#### OpenTheory ####################################################

OTPATH = export/opentheory
OTFILES=$(addprefix $(OTPATH)/,$(addsuffix .art,$(IMP)))
THYFILE=$(OTPATH)/$(PACKAGE).thy

$(OTPATH)/%.art: $(IPATH)/%.dko theories/$(THEORY).dko .library_depend_art $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) $(LOGIPEDIAOPTS) --export opentheory $(<:.dko=.dk) -o $@

.PHONY: opentheory
opentheory: $(OTFILES)
	$(PYTHON) bin/gen-thy-file.py $(DKDEP) $(IPATH) $(PACKAGE) > $(THYFILE)
	$(OT) info $(THYFILE) 2>/dev/null
	@echo "[OT] CHECKED"

##### PVS ##########################################################


PVSPATH = export/pvs
PVSFILES=$(addprefix $(PVSPATH)/,$(addsuffix .pvs,$(IMP)))
PVSSUM=$(addprefix $(PVSPATH)/,$(addsuffix .summary,$(IMP)))
# For some weird reason, Make consider .pvs are temporary
$(PVSPATH)/%.pvs: $(IPATH)/%.dko theories/$(THEORY).dko .library_depend_pvs $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) $(LOGIPEDIAOPTS) --export pvs $(<:.dko=.dk) -o $@

$(PVSPATH)/%.summary: $(PVSPATH)/%.pvs
	@echo "[SUMMARY] $@"
	$(PROVEIT) --importchain -sf $<

.PHONY: pvs
pvs: $(PVSSUM)
	@echo "[OT] CHECKED"

#### Json ##########################################################

jspath = export/json
jsfiles = $(addprefix $(jspath)/, $(addsuffix .json, $(IMP)))

export/json/%.json: $(IPATH)/%.dko $(LOGIPEDIA)
	@mkdir -p $(jspath)
	$(LOGIPEDIA) $(LOGIPEDIAOPTS) --export-json $(<:.dko=.dk) -o $@

.PHONY: json
json: $(jsfiles)

#### Dependencies ##################################################

.library_depend_dko: $(DKS) $(THDIR)/$(THEORY).dk
	@echo "[DKDEP (DK FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I $(THDIR) $^

.library_depend_v: $(wildcard $(IPATH)/*.dk theories/$(THEORY).dk)
	@echo "[DKDEP (V FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/v/g $@
	sed  -i "s:$(IPATH)/\([^/]\+\)\.v:$(COQPATH)/\1\.v:g" $@

.library_depend_ma: $(wildcard $(IPATH)/*.dk theories/$(THEORY).dk)
	@echo "[DKDEP (MA FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/ma/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.ma:$(MATITAPATH)/\1\.ma:g" $@

.library_depend_lean: $(wildcard $(IPATH)/*.dk theories/$(THEORY).dk)
	@echo "[DKDEP (LEAN FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/lean/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.lean:$(LEANPATH)/\1\.lean:g" $@

.library_depend_art: $(wildcard $(IPATH)/*.dk theories/$(THEORY).dk)
	@echo "[DKDEP (ART FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/art/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.art:$(OTPATH)/\1\.art:g" $@

.library_depend_pvs: $(wildcard $(IPATH)/*.dk theories/$(THEORY).dk)
	@echo "[DKDEP (PVS FILES)] $@"
	@$(DKDEP) -o $@ -I $(IPATH) -I theories $^
	@sed -i s/theories\\/sttfa.dko//g $@
	@sed -i s/dko/pvs/g $@
	sed  -i "s:$(IPATH)/\([^.]*\)\.pvs:$(PVSPATH)/\1\.pvs:g" $@

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
-include .library_depend_v
-include .library_depend_ma
-include .library_depend_lean
-include .library_depend_art
-include .library_depend_pvs
endif
endif

#### Cleaning targets ##############################################

.PHONY: clean
clean:
	@dune clean
	@$(RM) .library_depend_dko
	@$(RM) .library_depend_v
	@$(RM) .library_depend_vo
	@$(RM) .library_depend_ma
	@$(RM) .library_depend_lean
	@$(RM) .library_depend_pvs
	@$(RM) .library_depend_art

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
	@find . -name "*.art"        -exec rm {} \;
	@find . -name "*.thy"        -exec rm {} \;
	@find . -name "*.summary"    -exec rm {} \;
	@find . -name "*.beautified" -exec rm {} \;
	@find . -name ".pvscontext"  -exec rm {} \;
	@-$(RM) -r /tmp/fermat
	@-$(RM) -r $(DKIMP)/$(THEORY)
