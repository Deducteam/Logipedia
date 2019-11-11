Q = @

DKCHECK  = dkcheck
DKDEP    = dkdep
DKMETA   = dkmeta
UNIVERSO = universo
DKPRUNE  = dkprune
DKPSULER = dkpsuler

DEBUG = -l

PACKAGE = ./
THEORY  = ./

ROOT    = ./
IMPORT  = 1-prune/input/
FROM    = $(realpath $(ROOT)/$(IMPORT))/$(THEORY)/$(PACKAGE)
TO      = /tmp

FROM_FILES = $(wildcard $(FROM)/*.dk)
LIB_FILES  = $(notdir $(basename $(FROM_FILES)))

export

.PHONY: default
default: prune universo dkpsuler

PRUNE_DIR  = 1-prune
PRUNE_IN_FILES = $(LIB_FILES:%=$(PRUNE_DIR)/input/%.dk)
PRUNE_OUT_FILES = $(wildcard $(PRUNE_DIR)/output/*.dk)

$(PRUNE_DIR)/input:
	mkdir -p $(PRUNE_DIR)/input

.PHONY: prune
prune: $(PRUNE_IN_FILES)
	$(Q)$(MAKE) -C $(PRUNE_DIR)

UNIVERSO_DIR = 2-universo
UNIVERSO_IN_FILES = $(PRUNE_OUT_FILES:$(PRUNE_DIR)/output/%.dk=$(UNIVERSO_DIR)/input/%.dk)
UNIVERSO_OUT_FILES = $(PRUNE_OUT_FILES:$(PRUNE_DIR)/output/%.dk=$(UNIVERSO_DIR)/output/%.dk)

$(UNIVERSO_DIR)/input:
	mkdir -p $(UNIVERSO_DIR)/input

$(UNIVERSO_IN_FILES): $(PRUNE_OUT_FILES)
	$(Q)cp $(PRUNE_OUT_FILES) $(UNIVERSO_DIR)/input

.PHONY: universohack
universohack: $(UNIVERSO_IN_FILES)
	$(Q) time -o /tmp/total_time $(MAKE) -C $(UNIVERSO_DIR)

.PHONY: universo
universo: $(UNIVERSO_DIR)/input prune
	$(Q)$(MAKE) -C . universohack

DKPSULER_DIR = 3-univ-poly-match
DKPSULER_IN_FILES = $(UNIVERSO_OUT_FILES:$(UNIVERSO_DIR)/output/%.dk=$(DKPSULER_DIR)/input/%.dk)
DKPSULER_OUT_FILES = $(UNIVERSO_OUT_FILES:$(UNIVERSO_DIR)/output/%.dk=$(DKPSULER_DIR)/output/%.dk)

$(DKPSULER_DIR)/input:
	mkdir -p $(DKPSULER_DIR)/input

$(DKPSULER_IN_FILES): $(UNIVERSO_OUT_FILES)
	$(Q)cp $(UNIVERSO_OUT_FILES) $(DKPSULER_DIR)/input

.PHONY: dkpsulerhack
dkpsulerhack: $(DKPSULER_IN_FILES)
	$(Q)$(MAKE) -C $(DKPSULER_DIR)

.PHONY: dkpsuler
dkpsuler: $(DKPSULER_DIR)/input $(UNIVERSO_OUT_FILES)
	$(Q)$(MAKE) -C . dkpsulerhack

DKMETA_DIR = 4-inv-product
DKMETA_IN_FILES = $(DKPSULER_OUT_FILES:$(DKPSULER_DIR)/output/%.dk=$(DKMETA_DIR)/input/%.dk)
DKMETA_OUT_FILES = $(DKPSULER_OUT_FILES:$(DKPSULER_DIR)/output/%.dk=$(DKMETA_DIR)/output/%.dk)

$(DKMETA_DIR)/input:
	mkdir -p $(DKMETA_DIR)/input

$(DKMETA_IN_FILES): $(DKPSULER_OUT_FILES)
	$(Q)cp $(DKPSULER_OUT_FILES) $(DKMETA_DIR)/input

.PHONY: dkmetahack
dkmetahack: $(DKMETA_IN_FILES)
	$(Q)$(MAKE) -C $(DKMETA_DIR)

.PHONY: dkmeta
dkmeta: $(DKMETA_DIR)/input $(DKPSULER_OUT_FILES)
	$(Q)$(MAKE) -C . dkmetahack

.PHONY: debug
debug:
	echo $(UNIVERSO_DIR)

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
