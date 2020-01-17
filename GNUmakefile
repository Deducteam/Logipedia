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
THEORY ?= sttfa
# Name of the package used
PKG ?= arith_fermat
# Directory where files are exported
EXPDIR ?= export
# Additional flags passed to Dedukti (--eta)
DKFLAGS =
# Which ocaml middleware module to use (for json export)
MIDDLEWARE = sttfa

# Directory containing theory files
_thdir = theories/$(THEORY)
# All theory files without extension
_thfiles = $(notdir $(basename $(wildcard $(_thdir)/*.dk)))
# Path to dedukti source file
_dkimp = import/dedukti
# Full include path
_ipath = $(_dkimp)/$(THEORY)/$(PKG)
# Directory to store dependencies
_depdir = .depends
# Most used logipedia options
_logipediaopts = -I $(_ipath) -I $(_thdir)

#### Logipedia binary ##############################################

LOGIPEDIA = _build/install/default/bin/logipedia
DK2JSON = _build/install/default/bin/dk2json

.PHONY: all
all: bin

logipedia: bin
	@echo "[BUILD EXECUTABLE] logipedia"
	-$(RM) logipedia
	@ln -s $(LOGIPEDIA) logipedia

dk2json: bin
	-$(RM) $@
	@ln -s $(DK2JSON) $@

bin: $(LOGIPEDIA) $(LOGIPEDIA) $(DK2JSON)

.PHONY: $(LOGIPEDIA) $(LOGIPEDIA) $(DK2JSON)
$(LOGIPEDIA):
	@dune build
$(DK2JSON):
	@dune build

.PHONY: install
install:
	@dune install

.PHONY: doc
doc:
	dune build @doc
	mkdocs build

#### Producing the theory file #####################################

_thdepdir = $(_depdir)/_$(THEORY)
$(_thdepdir)/%.d: $(_thdir)/%.dk
	@mkdir -p $(@D)
	@$(DKDEP) -o $@ -I $(_thdir) $^

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include $(addprefix $(_thdepdir)/, $(_thfiles).d)
endif
endif

$(_thdir)/%.dko: $(_thdir)/%.dk $(_thdepdir/%.d)
	@mkdir -p $(@D)
	@echo "[CHECK] $^"
	@$(DKCHECK) $(DKFLAGS) -e -I $(_thdir)/ $^


#### Producing the Dedukti library #################################

## We untar the archive here to have the list of files available at first run of
## Make
## HACK files are stored on a lsv webpage, something better should be set
__dks := $(shell cd $(_dkimp) && \
if [ ! -d $(THEORY) ]; then \
curl http://www.lsv.fr/~hondet/logipedia/$(THEORY).tar.bz2 | \
tar xj ; fi &&\
cd $(THEORY)/$(PKG) && ls *.dk)
_dks := $(addprefix $(_ipath)/, $(__dks))
_dkos := $(patsubst %.dk,%.dko,$(_dks))
_srcbase := $(notdir $(basename $(_dks)))

$(_ipath)/%.dko: $(_ipath)/%.dk $(_thdir)/$(_thfiles:=.dko)
	@echo "[CHECK] $@"
	@$(DKCHECK) $(DKFLAGS) -e -I $(_thdir) -I $(_ipath) $<

.PHONY: dedukti
dedukti: $(_dkos)
	@echo "[DEDUKTI] CHECKED"

#### Coq ###########################################################

_coqpath = $(EXPDIR)/coq
_vfiles = $(addprefix $(_coqpath)/, $(addsuffix .v, $(_srcbase)))

$(_coqpath)/%.v: $(_ipath)/%.dko .library_depend_v $(LOGIPEDIA)
	@mkdir -p $(_coqpath)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) coq $(_logipediaopts) --fast -f $(<:.dko=.dk) -o $@
	@mv $@ $(addsuffix .v, $(subst -,_, $(subst .,_,$(basename $@)))) \
|| true 2>/dev/null # avoid fail if there is no change

$(_coqpath)/_CoqProject: $(_vfiles)
	@cd $(_coqpath) && ls *.v > _CoqProject

$(_coqpath)/Makefile: $(_coqpath)/_CoqProject
	@cd $(_coqpath) && coq_makefile -f _CoqProject -o Makefile

.PHONY: coq
coq: $(_vfiles)
# coq: $(_coqpath)/Makefile $(_vfiles)
# 	@cd $(_coqpath) && make
# 	@echo "[COQ] CHECKED"


#### Matita ########################################################

_matitapath = $(EXPDIR)/matita
_mafiles=$(addprefix $(_matitapath)/, $(addsuffix .ma, $(_srcbase)))

$(_matitapath)/%.ma: $(_ipath)/%.dko .library_depend_ma $(LOGIPEDIA)
	@mkdir -p $(_matitapath)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) matita $(_logipediaopts) -f $(<:.dko=.dk) -o $@

$(_matitapath)/root:
	@echo "baseuri = cic:/matita" > $@

.PHONY: matita
matita: $(_mafiles)
# matita: $(_mafiles) $(_matitapath)/root
# 	@cd $(_matitapath) && $(MATITAC) *.ma
# 	@echo "[MATITA] CHECKED"

#### Lean ##########################################################

_leanpath = $(EXPDIR)/lean
_leanfiles=$(addprefix $(_leanpath)/,$(addsuffix .lean,$(_srcbase)))

$(_leanpath)/%.lean: $(_ipath)/%.dko .library_depend_lean $(LOGIPEDIA)
	@mkdir -p $(_leanpath)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) lean $(_logipediaopts) -f $(<:.dko=.dk) -o $@

.PHONY: lean
lean: $(_leanfiles)
	@cd $(_leanpath) && $(LEAN) *.lean
	@echo "[LEAN] CHECKED"

#### OpenTheory ####################################################

_otpath = $(EXPDIR)/opentheory
_otfiles=$(addprefix $(_otpath)/,$(addsuffix .art,$(_srcbase)))
_thyfile=$(_otpath)/$(PKG).thy

$(_otpath)/%.art: $(_ipath)/%.dko .library_depend_art $(LOGIPEDIA)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) opentheory $(_logipediaopts) -f $(<:.dko=.dk) -o $@

.PHONY: opentheory
opentheory: $(_otfiles)
# opentheory: $(_otfiles)
# 	$(PYTHON) bin/gen-thy-file.py $(DKDEP) $(_ipath) $(PKG) > $(_thyfile)
# 	$(OT) info $(_thyfile) 2>/dev/null
# 	@echo "[OT] CHECKED"

#### HOL Light ######################################################

_holpath = $(EXPDIR)/hollight
_holfiles=$(addprefix $(_holpath)/,$(addsuffix .ml,$(_srcbase)))

$(_holpath)/%.ml: $(_ipath)/%.dko .library_depend_hol $(LOGIPEDIA)
	@mkdir -p $(_holpath)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) hollight $(_logipediaopts) -f $(<:.dko=.dk) -o $@

.PHONY: hollight
hollight: $(_holfiles)
	@echo "[HOL] FILES TO BE CHECKED"


##### PVS ##########################################################


_pvspath = $(EXPDIR)/pvs
_pvsfiles=$(addprefix $(_pvspath)/,$(addsuffix .pvs,$(_srcbase)))
_pvssum=$(addprefix $(_pvspath)/,$(addsuffix .summary,$(_srcbase)))
# For some weird reason, Make consider .pvs are temporary
$(_pvspath)/%.pvs: $(_ipath)/%.dko .library_depend_pvs $(LOGIPEDIA)
	@mkdir -p $(_pvspath)
	@echo "[EXPORT] $@"
	@$(LOGIPEDIA) pvs $(_logipediaopts) -f $(<:.dko=.dk) -o $@

$(_pvspath)/%.summary: $(_pvspath)/%.pvs
	@echo "[SUMMARY] $@"
# proveit always return 1
	@true || $(PROVEIT) --importchain --scripts --force $<

.PHONY: pvs
pvs: $(_pvssum)
	@echo "[PVS] CHECKED"

#### Json ##########################################################

_jsonpath = $(EXPDIR)/json
_jsonthpath = $(_jsonpath)/_theory
_jsonfiles = $(addprefix $(_jsonpath)/, $(addsuffix .json, $(_srcbase)))

$(_jsonthpath)/%.json: $(_thdir)/%.dko $(DK2JSON)
	@mkdir -p $(_jsonpath)/_theory
	$(DK2JSON) --hollight $(EXPDIR)/hollight --lean $(EXPDIR)/lean --pvs $(EXPDIR)/pvs   --matita $(EXPDIR)/matita  --coq $(EXPDIR)/coq $(_logipediaopts) -m $(MIDDLEWARE) -f $(<:.dko=.dk) -o $@

$(_jsonpath)/%.json: $(_ipath)/%.dko $(DK2JSON)
	$(DK2JSON) --hollight $(EXPDIR)/hollight --lean $(EXPDIR)/lean --pvs $(EXPDIR)/pvs  --matita $(EXPDIR)/matita --coq $(EXPDIR)/coq $(_logipediaopts) -m $(MIDDLEWARE) -f $(<:.dko=.dk) -o $@

.PHONY: json
json: $(addprefix $(_jsonthpath)/, $(_thfiles:=.json)) $(_jsonfiles)

#### Dependencies ##################################################

_esc_thdir = $(subst /,\\/,$(_thdir))
.library_depend_dko: $(_dks) $(_thdir)/$(_thfiles).dk
	@echo "[DKDEP (DK FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^

_esc_coqpath = $(subst /,\\/,$(_coqpath))
.library_depend_v: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (V FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_coqpath)\\/$$f/ $@ ; \
	done
	@sed -i s/dko/v/g $@
	@sed -i s/dk/dko/g $@
	@sed -i "s:$(_ipath)/\([^/]\+\)\.v:$(_coqpath)/\1\.v:g" $@

_esc_matitapath = $(subst /,\\/,$(_matitapath))
.library_depend_ma: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (MA FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_matitapath)\\/$$f/ $@ ; \
	done
	@sed -i s/dko/ma/g $@
	@sed -i s/dk/dko/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.ma:$(_matitapath)/\1\.ma:g" $@

_esc_leanpath = $(subst /,\\/,$(_leanpath))
.library_depend_lean: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (LEAN FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_leanpath)\\/$$f/ $@ ; \
	done
	@sed -i s/dko/lean/g $@
	@sed -i s/dk/dko/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.lean:$(_leanpath)/\1\.lean:g" $@

_esc_otpath = $(subst /,\\/,$(_otpath))
.library_depend_art: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (ART FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_otpath)\\/$$f/ $@ ; \
	done
	@sed -i s/dko/art/g $@
	@sed -i s/dk/dko/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.art:$(_otpath)/\1\.art:g" $@

_esc_holpath = $(subst /,\\/,$(_holpath))
.library_depend_hol: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (HOLLIGHT FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_holpath)\\/$$f/ $@ ; \
	done
	@sed -i s/dko/ml/g $@
	@sed -i s/dk/dko/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.ml:$(_holpath)/\1\.ml:g" $@

_esc_pvspath = $(subst /,\\/,$(_pvspath))
.library_depend_pvs: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (PVS FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_leanpath)\\/$$f/ $@ ; \
	done
	@sed -i s/dko/pvs/g $@
	@sed -i s/dk/dko/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.pvs:$(_pvspath)/\1\.pvs:g" $@

_esc_jsonpath = $(subst /,\\/,$(_jsonpath))
.library_depend_json: $(wildcard $(_ipath)/*.dk $(_thdir)/$(_thfiles).dk)
	@echo "[DKDEP (JSON FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
	@for f in $(addsuffix .dko, $(_thfiles)) ; do \
		sed -i s/$(_esc_thdir)\\/$$f/$(_esc_jsonpath)\\/theory\\/$$f/ $@ ; \
	done
	@sed -i s/dko/json/g $@
	@sed -i s/dk/dko/g $@
	@sed  -i "s:$(_ipath)/\([^.]*\)\.json:$(_jsonpath)/\1\.json:g" $@

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
-include .library_depend_v
-include .library_depend_ma
-include .library_depend_lean
-include .library_depend_art
-include .library_depend_pvs
-include .library_depend_json
-include .library_depend_hol
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
	@$(RM) -r $(_depdir)
	@$(RM) .library_depend_dko
	@$(RM) .library_depend_v
	@$(RM) .library_depend_vo
	@$(RM) .library_depend_ma
	@$(RM) .library_depend_lean
	@$(RM) .library_depend_pvs
	@$(RM) .library_depend_art
	@$(RM) .library_depend_hol


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
	@-$(RM) -r $(EXPDIR)
	@-$(RM) -r /tmp/fermat
