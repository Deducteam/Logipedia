Q = @

DKCHECK  = dkcheck
DKDEP    = dkdep
DKMETA   = dkmeta
UNIVERSO = universo
DKPRUNE  = dkprune
DKPSULER = dkpsuler

DEBUG = -l

PACKAGE = arith_matita
THEORY  = cts

ROOT    = ../..
IMPORT  = import/dedukti
FROM    = $(realpath $(ROOT)/$(IMPORT))/$(THEORY)/$(PACKAGE)
TO      = /tmp

FROM_FILES = $(wildcard $(FROM)/*.dk)
LIB_FILES  = $(notdir $(basename $(FROM_FILES)))

export

.PHONY: default
default: prune universo dkpsuler

$(PRUNE_DIR)/input:
	mkdir -p $(PRUNE_DIR)/input

$(FROM):
	cd $(realpath $(ROOT)/$(IMPORT)) && tar xvjf $(THEORY).tar.bz2


PRUNE_DIR  = 1-prune
PRUNE_IN_FILES = $(LIB_FILES:%=$(PRUNE_DIR)/input/%.dk)
PRUNE_OUT_FILES = $(wildcard $(PRUNE_DIR)/output/*.dk)

$(PRUNE_IN_FILES): $(PRUNE_DIR)/input/%.dk: $(PRUNE_DIR)/input $(FROM_FILES)
	$(Q)cp $(FROM)/$*.dk $(PRUNE_DIR)/input

.PHONY: prune
prune: $(FROM) $(PRUNE_IN_FILES)
	$(Q)$(MAKE) -C $(PRUNE_DIR)

UNIVERSO_DIR = 2-universo
UNIVERSO_IN_FILES = $(PRUNE_OUT_FILES:$(PRUNE_DIR)/output/%.dk=$(UNIVERSO_DIR)/input/%.dk)
UNIVERSO_OUT_FILES = $(PRUNE_OUT_FILES:$(PRUNE_DIR)/output/%.dk=$(UNIVERSO_DIR)/output/%.dk)

$(UNIVERSO_IN_FILES): $(PRUNE_OUT_FILES)
	$(Q)cp $(PRUNE_OUT_FILES) $(UNIVERSO_DIR)/input

.PHONY: universohack
universohack: $(UNIVERSO_IN_FILES)
	$(Q)$(MAKE) -C $(UNIVERSO_DIR)

.PHONY: universo
universo: prune
	$(Q)$(MAKE) -C . universohack

DKPSULER_DIR = 3-univ-poly-match
DKPSULER_IN_FILES = $(UNIVERSO_OUT_FILES:$(UNIVERSO_DIR)/output/%.dk=$(DKPSULER_DIR)/input/%.dk)
DKPSULER_OUT_FILES = $(UNIVERSO_OUT_FILES:$(UNIVERSO_DIR)/output/%.dk=$(DKPSULER_DIR)/output/%.dk)

$(DKPSULER_IN_FILES): $(UNIVERSO_OUT_FILES)
	$(Q)cp $(UNIVERSO_OUT_FILES) $(DKPSULER_DIR)/input

.PHONY: dkpsulerhack
dkpsulerhack: $(DKPSULER_IN_FILES)
	$(Q)$(MAKE) -C $(DKPSULER_DIR)

.PHONY: dkpsuler
dkpsuler: $(UNIVERSO_OUT_FILES)
	$(Q)$(MAKE) -C . dkpsulerhack

.PHONY: debug
debug:
	echo $(realpath $(ROOT)/$(IMPORT))/$(THEORY)/$(PACKAGE)

.PHONY: clean
clean:
	$(Q)$(MAKE) -C $(PRUNE_DIR) clean
	$(Q)$(MAKE) -C $(UNIVERSO_DIR) clean
	$(Q)$(MAKE) -C $(DKPSULER_DIR) clean

.PHONY: distclean
distclean:
	$(Q)$(MAKE) -C $(PRUNE_DIR) distclean
	$(Q)$(MAKE) -C $(UNIVERSO_DIR) distclean
	$(Q)$(MAKE) -C $(DKPSULER_DIR) distclean
