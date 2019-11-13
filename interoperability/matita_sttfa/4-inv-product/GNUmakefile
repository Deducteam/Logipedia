INPUT=input
OUTPUT=output

IN_FILES=$(wildcard $(INPUT)/*.dk)
LIB=$(notdir $(basename $(IN_FILES)))

DKMETA_INPUT_FILES  = $(LIB:%=$(INPUT)/%.dk)
DKMETA_OUTPUT_FILES = $(LIB:%=$(OUTPUT)/%.dk)

META=meta.dk


.PHONY: default
default: $(DKMETA_OUTPUT_FILES)

$(OUTPUT):
	mkdir -p $(OUTPUT)

$(DKMETA_OUTPUT_FILES): $(OUTPUT)/%.dk: $(INPUT)/%.dk $(META) $(OUTPUT)
	dkmeta --encoding prod -m $(META) $< > $@


.PHONY: clean
clean:
	$(Q)rm $(OUTPUT)/*.dk 2> /dev/null || true

.PHONY: distclean
distclean: clean
	$(Q)rm $(INPUT)/*.dk 2> /dev/null || true
