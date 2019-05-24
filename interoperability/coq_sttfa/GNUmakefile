Q = @

DKCHECK  = dkcheck
DKDEP    = dkdep
DKMETA   = dkmeta
UNIVERSO = universo
DKPRUNE  = dkprune

DEBUG = -l

PACKAGE = geocoq
THEORY  = coq

ROOT    = ../..
IMPORT  = import/dedukti
FROM    = $(realpath $(ROOT)/$(IMPORT)/$(THEORY)/$(PACKAGE))
TO      = /tmp

FROM_FILES = $(wildcard $(FROM)/*.dk)
LIB_FILES  = $(notdir $(basename $(FROM_FILES)))

export

.PHONY: default
default: prune universo

PRUNE_DIR  = 1-prune
PRUNE_IN_FILES = $(LIB_FILES:%=$(PRUNE_DIR)/input/%.dk)
PRUNE_OUT_FILES = $(wildcard $(PRUNE_DIR)/output/*.dk)

$(PRUNE_IN_FILES): $(PRUNE_DIR)/input/%.dk: $(FROM_FILES)
	$(Q)cp $(FROM)/$*.dk $(PRUNE_DIR)/input

.PHONY: prune
prune: $(PRUNE_IN_FILES)
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

.PHONY: clean
clean:
	$(Q)$(MAKE) -C $(PRUNE_DIR) clean
	$(Q)$(MAKE) -C $(UNIVERSO_DIR) clean

.PHONY: distclean
distclean:
	$(Q)$(MAKE) -C $(PRUNE_DIR) distclean
	$(Q)$(MAKE) -C $(UNIVERSO_DIR) distclean

.PHONY: debug
debug:
	@echo $(UNIVERSO_IN_FILES)
