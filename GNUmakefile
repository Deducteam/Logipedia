DKCHECK = dkcheck
DKDEP   = dkdep
PYTHON  = python3

## In uppercase are variables that can be modified by the user, in lowercase
## with _ are variables that are not supposed to be chosen by the user.
# Name of the theories used
THEORY ?= sttfa
# Name of the package used
PKG ?= arith_fermat
# Additional flags passed to Logipedia
LOGIPEDIAFLAGS =

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

#### Logipedia binary ##############################################

LOGIPEDIA = _build/install/default/bin/logipedia
DK2JSON = _build/install/default/bin/dk2json

.PHONY: all
all: bin

logipedia: bin
	@-$(RM) $@
	@ln -s $(LOGIPEDIA) logipedia

dk2json: bin
	@-$(RM) $@
	@ln -s $(DK2JSON) $@

bin: $(LOGIPEDIA) $(DK2JSON)

.PHONY: $(LOGIPEDIA) $(DK2JSON)
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

#### Checking Dedukti #####################################
_thdepdir = $(_depdir)/_$(THEORY)
$(_thdepdir)/%.d: $(_thdir)/%.dk
	@mkdir -p $(@D)
	@$(DKDEP) -o $@ -I $(_thdir) $^

$(_thdir)/%.dko: $(_thdir)/%.dk $(_thdepdir)/%.d
	@mkdir -p $(@D)
	@echo "[CHECK] $^"
	@$(DKCHECK) $(DKFLAGS) -e -I $(_thdir)/ $<
_dkodepdir = $(_depdir)/$(_ipath)

$(_dkodepdir)/%.d: $(_ipath)/%.dk
	@mkdir -p $(_dkodepdir)
	$(DKDEP) -o $@ -I $(_ipath) -I $(_thdir) $^
depfiles = $(patsubst %.dk, $(_dkodepdir)/%.d, $(__dks))

$(_ipath)/%.dko: $(_ipath)/%.dk $(_thdir)/$(_thfiles:=.dko) $(depfiles)
	@echo "[CHECK] $@"
	$(DKCHECK) $(DKFLAGS) -e -I $(_thdir) -I $(_ipath) $<
.PHONY: dedukti
dedukti: $(_dkos)
	@echo "[DEDUKTI] CHECKED"

ifeq ($(MAKECMDGOALS), dedukti)
-include $(addprefix $(_thdepdir)/, $(_thfiles).d)
-include $(depfiles)
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
	@$(RM) -f *.lpdb
	@$(RM) -f .*.lpdb
	@$(RM) dk2json
	@$(RM) logipedia

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
