INPUT=input
OUTPUT=output

IN_FILES=$(wildcard $(INPUT)/*.dk)
LIB=$(notdir $(basename $(IN_FILES)))

DKPSULER_INPUT_FILES  = $(LIB:%=$(INPUT)/%.dk)
DKPSULER_OUTPUT_FILES = $(LIB:%=$(OUTPUT)/%.dk)
DKPSULER_O_OUTPUT_FILES = $(LIB:%=$(OUTPUT)/%.dko)

IN_DEP_FILES=$(LIB:%=$(INPUT)/%.dep)
OUT_DEP_FILES=$(LIB:%=$(OUTPUT)/%.dep)

CONFIG=config.dk

INDUCTIVESH=./inductives.sh

SHELL := /bin/bash

.PHONY: default
default: $(DKPSULER_O_OUTPUT_FILES)

$(OUTPUT):
	mkdir -p $(OUTPUT)

$(DKPSULER_OUTPUT_FILES): $(OUTPUT)/%.dk: $(INPUT)/%.dk $(OUTPUT)/%.dep $(CONFIG) cic.dko $(OUTPUT)
	$(Q)$(DKPSULER) $(DEBUG) --config $(CONFIG) -o $(OUTPUT) -I $(OUTPUT) $<

$(DKPSULER_O_OUTPUT_FILES): $(OUTPUT)/%.dko: $(OUTPUT)/%.dk
	$(Q)$(DKCHECK) -e -I $(OUTPUT) $<

$(IN_DEP_FILES): $(INPUT)/%.dep: $(INPUT)/%.dk
	$(Q)$(DKDEP) -I $(INPUT) $< > $@

$(OUT_DEP_FILES): $(OUTPUT)/%.dep: $(INPUT)/%.dep
	$(Q)cat $< | cut -d: -f2- | sed "s/^\s*//g" \
				  | sed "s/\(\ \)/: /" \
				  | sed 's:$(INPUT)/:$(OUTPUT)/:g' > $@

cic.dko: cic.dk
	dkcheck -e cic.dk

ifneq ($(MAKECMDGOALS), clean distclean)
-include $(OUT_DEP_FILES)
endif

$(CONFIG): $(DKPSULER_INPUT_FILES)
	$(shell $(INDUCTIVESH) $(DKPSULER_INPUT_FILES) > $(CONFIG))

.PHONY: debug
debug:
	echo $(DKPSULER_OUTPUT_FILES)

.PHONY: clean
clean:
	$(Q)rm $(OUTPUT)/*.dk 2> /dev/null || true

.PHONY: distclean
distclean: clean
	$(Q)rm $(INPUT)/*.dk 2> /dev/null || true
