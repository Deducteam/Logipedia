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
	@dune build @doc

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
.PHONY: coq
coq: $(LOGIPEDIA) $(_dkos)
	@mkdir -p $(_coqpath)
	$(LOGIPEDIA) coq -I $(_thdir) -I $(_ipath) -o $(_coqpath) \
-d $(_ipath)
	cd $(_coqpath) && rename 's:-:_:g' $(_coqpath)/*.v
	cd $(_coqpath) && rename 's:\.:_:g' $(_coqpath)/*.v
	@cd $(_coqpath) && ls *.v > _CoqProject
	@cd $(_coqpath) && coq_makefile -f _CoqProject -o Makefile
	@cd $(_coqpath) && $(MAKE)
	@echo "[COQ] CHECKED"


#### Matita ########################################################
_matitapath = $(EXPDIR)/matita
.PHONY: matita
matita: $(LOGIPEDIA) $(_matitapath)/root $(_dkos)
	@mkdir -p $(_matitapath)
	@echo "baseuri = cic:/matita" > $(_matitapath)/root
	$(LOGIPEDIA) matita -I $(_thdir) -I $(_ipath) -o $(_matitapath) \
-d $(_ipath)
	@cd $(_matitapath) && $(MATITAC) *.ma
	@echo "[MATITA] CHECKED"

#### Lean ##########################################################
_leanpath = $(EXPDIR)/lean

.PHONY: lean
lean: $(_dkos) $(LOGIPEDIA)
	@mkdir -p $(_leanpath)
	$(LOGIPEDIA) lean -I $(_thdir) -I $(_ipath) -o $(_leanpath) -d $(_ipath)
	@cd $(_leanpath) && $(LEAN) *.lean
	@echo "[LEAN] CHECKED"

#### OpenTheory ####################################################
_otpath = $(EXPDIR)/opentheory
_thyfile=$(_otpath)/$(PKG).thy

.PHONY: opentheory
opentheory: $(LOGIPEDIA) $(_dkos)
	$(LOGIPEDIA) opentheory -I $(_thdir) -I $(_ipath) -o $(_otpath) \
-d $(_ipath)
	$(PYTHON) bin/gen-thy-file.py $(DKDEP) $(_ipath) $(PKG) > $(_thyfile)
	$(OT) info $(_thyfile) 2>/dev/null
	@echo "[OT] CHECKED"

#### HOL Light ######################################################
_holpath = $(EXPDIR)/hollight
hollight: $(_dkos) $(LOGIPEDIA)
	@mkdir -p $(_holpath)
	$(LOGIPEDIA) hollight -I $(_thdir) -I $(_ipath) -o $(_holpath) \
-d $(_ipath)
	@echo "[HOL] FILES TO BE CHECKED"

##### PVS ##########################################################
_pvspath = $(EXPDIR)/pvs
_pvssum=$(addprefix $(_pvspath)/,$(addsuffix .summary,$(_srcbase)))

.PHONY: pvs
pvs: $(_dkos) $(LOGIPEDIA)
	@mkdir -p $(_pvspath)
	$(LOGIPEDIA) pvs -I $(_thdir) -I $(_ipath) -o $(_pvspath) -d $(_ipath)
	for file in $(wildcard $(_pvspath/*.pvs)); do \
		$(PROVEIT) --importchain --scripts --force $$file ; \
	done
	@echo "[PVS] CHECKED"

#### Json ##########################################################
_jsonpath = $(EXPDIR)/json
_thfiles = $(wildcard $(_thdir)/*.dk)

.PHONY: json
json: dedukti $(DK2JSON)
	@mkdir -p $(_jsonpath)
	$(DK2JSON) -m $(MIDDLEWARE) -o $(_jsonpath) -J $(_jsonpath) \
-I $(_ipath) -d $(_ipath) -I $(_thdir) $(_thfiles) \
--hollight $(_holpath) --pvs $(_pvspath) --lean $(_leanpath)

#### Dependencies ##################################################

_esc_thdir = $(subst /,\\/,$(_thdir))
.library_depend_dko: $(_dks) $(_thdir)/$(_thfiles).dk
	@echo "[DKDEP (DK FILES)] $@"
	@$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^

ifneq ($(MAKECMDGOALS), clean)
ifneq ($(MAKECMDGOALS), distclean)
-include .library_depend_dko
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
