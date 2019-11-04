DKCHECK = dkcheck
DKDEP   = dkdep
MATITAC = matitac
LEAN    = lean
PYTHON  = python3
OT      = opentheory
PROVEIT = proveit

## In uppercase are variables that can be modified by the user, in lowercase
## with _ are variables that are not supposed to be chosen by the user.
# Name of the theories used
THEORY ?=
# Name of the package used
PKG ?=
# Directory where files are exported
EXPDIR ?=

# Directory containing theory files
_thdir = theories
# Path to dedukti source file
_dkimp = import/dedukti
# Full include path
_ipath = $(_dkimp)/$(THEORY)/$(PKG)

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

$(_thdir)/hol_sttfa.dko: $(_thdir)/hol_sttfa.dk $(_thdir)/sttfa.dko \
$(_thdir)/hol.dko $(_thdir)/hol_axioms.dko
	@echo "[CHECK] $<"
	@$(DKCHECK) -I $(_thdir) -e $<

$(_thdir)/hol_axioms.dko: $(_thdir)/hol_axioms.dk $(_thdir)/hol.dko
	@echo "[CHECK] $<"
	@$(DKCHECK) -I $(_thdir) -e $<

$(_thdir)/%.dko: $(_thdir)/%.dk
	@echo "[CHECK] $^"
	@$(DKCHECK) -e -I $(_thdir)/ $^


#### Producing the Dedukti library #################################

LOGIPEDIAOPTS = -I $(_ipath) -I $(_thdir)
$(info [IMPORT] Using package ${PKG})
$(info [EXPORT] Exporting to ${EXPDIR})

#### Dedukti #######################################################

## We untar the archive here to have the list of files available at first run of
## Make
## HACK files are stored on a lsv webpage, something better should be set
_dks := $(shell cd $(_dkimp) && \
if [ ! -d $(THEORY) ]; then \
curl http://www.lsv.fr/~hondet/logipedia/$(THEORY).tar.bz2 | \
tar xj ; fi &&\
cd $(THEORY)/$(PKG) && ls *.dk)
_dks := $(addprefix $(_ipath)/, $(_dks))
_dkos := $(patsubst %.dk,%.dko,$(_dks))
_srcbase := $(notdir $(basename $(_dks)))

$(_ipath)/%.dko: $(_ipath)/%.dk $(_thdir)/$(THEORY).dko
	@echo "[CHECK] $@"
	@$(DKCHECK) -e -I $(_thdir) -I $(_ipath) $<

.PHONY: dedukti
dedukti: $(_dkos)
	@echo "[DEDUKTI] CHECKED"

#### Coq ###########################################################

COQPATH = $(EXPDIR)/coq
VFILES = $(addprefix $(COQPATH)/, $(addsuffix .v, $(_srcbase)))

$(COQPATH)/%.v: $(_ipath)/%.dko $(_thdir)/$(THEORY).dko .library_depend_v \
$(LOGIPEDIA)
	@mkdir -p $(COQPATH)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) coq $(LOGIPEDIAOPTS) --fast -f $(<:.dko=.dk) -o $@
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

MATITAPATH = $(EXPDIR)/matita
MAFILES=$(addprefix $(MATITAPATH)/, $(addsuffix .ma, $(_srcbase)))

$(MATITAPATH)/%.ma: $(_ipath)/%.dko $(_thdir)/$(THEORY).dko .library_depend_ma $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) matita $(LOGIPEDIAOPTS) -f $(<:.dko=.dk) -o $@

$(MATITAPATH)/root:
	@echo "baseuri = cic:/matita" > $@

.PHONY: matita
matita: $(MAFILES) $(MATITAPATH)/root
	@cd $(MATITAPATH) && $(MATITAC) *.ma
	@echo "[MATITA] CHECKED"

#### Lean ##########################################################

LEANPATH = $(EXPDIR)/lean
LEANFILES=$(addprefix $(LEANPATH)/,$(addsuffix .lean,$(_srcbase)))

$(LEANPATH)/%.lean: $(_ipath)/%.dko $(_thdir)/$(THEORY).dko .library_depend_lean $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) lean $(LOGIPEDIAOPTS) -f $(<:.dko=.dk) -o $@

.PHONY: lean
lean: $(LEANFILES)
	@cd $(LEANPATH) && $(LEAN) *.lean
	@echo "[LEAN] CHECKED"

#### OpenTheory ####################################################

OTPATH = $(EXPDIR)/opentheory
OTFILES=$(addprefix $(OTPATH)/,$(addsuffix .art,$(_srcbase)))
THYFILE=$(OTPATH)/$(PKG).thy

$(OTPATH)/%.art: $(_ipath)/%.dko $(_thdir)/$(THEORY).dko .library_depend_art $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) opentheory $(LOGIPEDIAOPTS) -f $(<:.dko=.dk) -o $@

.PHONY: opentheory
opentheory: $(OTFILES)
	$(PYTHON) bin/gen-thy-file.py $(DKDEP) $(_ipath) $(PKG) > $(THYFILE)
	$(OT) info $(THYFILE) 2>/dev/null
	@echo "[OT] CHECKED"

##### PVS ##########################################################


PVSPATH = $(EXPDIR)/pvs
PVSFILES=$(addprefix $(PVSPATH)/,$(addsuffix .pvs,$(_srcbase)))
PVSSUM=$(addprefix $(PVSPATH)/,$(addsuffix .summary,$(_srcbase)))
# For some weird reason, Make consider .pvs are temporary
$(PVSPATH)/%.pvs: $(_ipath)/%.dko $(_thdir)/$(THEORY).dko .library_depend_pvs $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) pvs $(LOGIPEDIAOPTS) -f $(<:.dko=.dk) -o $@

$(PVSPATH)/%.summary: $(PVSPATH)/%.pvs
	@echo "[SUMMARY] $@"
# proveit always return 1
	@true || $(PROVEIT) --importchain --scripts --force $<

.PHONY: pvs
pvs: $(PVSSUM)
	@echo "[PVS] CHECKED"

#### Json ##########################################################
## Needs _thdir, THEORY, _ipath, _srcbase

jspath = $(EXPDIR)/json
jsfiles = $(addprefix $(jspath)/, $(addsuffix .json, $(_srcbase)))

$(EXPDIR)/json/$(THEORY).json: $(_thdir)/$(THEORY).dk
	@mkdir -p $(jspath)
	$(LOGIPEDIA) json -I $(_thdir) -f $< -o $@

$(EXPDIR)/json/%.json: $(_ipath)/%.dko $(LOGIPEDIA) export/json/$(THEORY).json
	@mkdir -p $(jspath)
	$(LOGIPEDIA) json $(LOGIPEDIAOPTS) -f $(<:.dko=.dk) -o $@

.PHONY: json
json: $(jsfiles)

#### Dependencies ##################################################

.library_depend_dko: $(_dks) $(_thdir)/$(THEORY).dk
	@echo "[DKDEP (DK FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^

.library_depend_v: $(wildcard $(_ipath)/*.dk $(_thdir)/$(THEORY).dk)
	@echo "[DKDEP (V FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@sed -i s/$(_thdir)\\/sttfa.dko//g $@
	@sed -i s/dko/v/g $@
	@sed  -i "s:$(_ipath)/\([^/]\+\)\.v:$(COQPATH)/\1\.v:g" $@

.library_depend_ma: $(wildcard $(_ipath)/*.dk $(_thdir)/$(THEORY).dk)
	@echo "[DKDEP (MA FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@sed -i s/$(_thdir)\\/sttfa.dko//g $@
	@sed -i s/dko/ma/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.ma:$(MATITAPATH)/\1\.ma:g" $@

.library_depend_lean: $(wildcard $(_ipath)/*.dk $(_thdir)/$(THEORY).dk)
	@echo "[DKDEP (LEAN FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@sed -i s/$(_thdir)\\/sttfa.dko//g $@
	@sed -i s/dko/lean/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.lean:$(LEANPATH)/\1\.lean:g" $@

.library_depend_art: $(wildcard $(_ipath)/*.dk $(_thdir)/$(THEORY).dk)
	@echo "[DKDEP (ART FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@sed -i s/$(_thdir)\\/sttfa.dko//g $@
	@sed -i s/dko/art/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.art:$(OTPATH)/\1\.art:g" $@

.library_depend_pvs: $(wildcard $(_ipath)/*.dk $(_thdir)/$(THEORY).dk)
	@echo "[DKDEP (PVS FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@sed -i s/$(_thdir)\\/sttfa.dko//g $@
	@sed -i s/dko/pvs/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.pvs:$(PVSPATH)/\1\.pvs:g" $@

.library_depend_json: $(wildcard $(_ipath)/*.dk $(_thdir)/$(THEORY).dk)
	@echo "[DKDEP (JSON FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@sed -i s/dko/json/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.json:$(jspath)/\1\.json:g" $@

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
-include .library_depend_v
-include .library_depend_ma
-include .library_depend_lean
-include .library_depend_art
-include .library_depend_pvs
-include .library_depend_json
endif
endif

#### Pretty printer ################################################

# FIXME logipp-latex definitve?
PP ?= /usr/local/bin/logipp-latex

.PHONY: pp
install_pp: $(PP)

.PHONY: $(PP)
$(PP):
	$(shell utils/install-pp.sh)

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
