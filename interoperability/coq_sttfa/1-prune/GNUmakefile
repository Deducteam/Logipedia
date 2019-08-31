INPUT=input
OUTPUT=output

PRUNE_INPUT_FILES  = $(LIB_FILES:%=$(INPUT)/%.dk)
PRUNE_OUTPUT_FILES = $(wildcard $(OUTPUT)/*.dk)
ifeq ($(PRUNE_OUTPUT_FILES),)
PRUNE_OUTPUT_FILES = $(LIB_FILES:%=$(OUTPUT)/%.dk)
endif

# dkprune can erase file, so probably the target might be buggy
$(PRUNE_OUTPUT_FILES): $(PRUNE_INPUT_FILES)
	$(Q)$(DKPRUNE) $(DEBUG) -o $(OUTPUT) -I $(INPUT) config.dk

.PHONY: clean
clean:
	$(Q)rm $(OUTPUT)/*.dk 2> /dev/null || true

.PHONY: distclean
distclean: clean
	$(Q)rm $(INPUT)/*.dk 2> /dev/null || true
