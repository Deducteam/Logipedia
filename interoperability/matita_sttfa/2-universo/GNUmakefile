Q = @

INPUT=input
OUTPUT=output

IN_FILES=$(wildcard $(INPUT)/*.dk)
LIB=$(notdir $(basename $(IN_FILES)))

UNIV_FILES=$(LIB:%=$(OUTPUT)/%_univ.dk)
UNIV_O_FILES=$(LIB:%=$(OUTPUT)/%_univ.dko)

CSTR_FILES=$(LIB:%=$(OUTPUT)/%_cstr.dk)
SOL_FILES=$(LIB:%=$(OUTPUT)/%_sol.dk)

OUT_FILES=$(LIB:%=$(OUTPUT)/%.dk)
OUT_O_FILES=$(LIB:%=$(OUTPUT)/%.dko)

IN_DEP_FILES=$(LIB:%=$(INPUT)/%.dep)
OUT_DEP_FILES=$(LIB:%=$(OUTPUT)/%.dep)

THEORY=cic.dk
CONFIG=config.dk

UNIVERSO_OPTIONS=\
	$(DEBUG) \
	--theory $(THEORY) \
	--config $(CONFIG)

.PHONY: default
default: $(SOL_FILES)

$(UNIV_FILES): $(OUTPUT)/%_univ.dk: $(OUTPUT)/%.dk

$(UNIV_O_FILES): $(OUTPUT)/%_univ.dko: $(OUTPUT)/%_univ.dk
	$(Q)$(DKCHECK) -e -I $(dir $(THEORY)) $<

$(CSTR_FILES): $(OUTPUT)/%_cstr.dk: $(OUTPUT)/%.dko

$(SOL_FILES): $(OUTPUT)/%_sol.dk: $(CSTR_FILES) $(OUTPUT)/%.dk
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --solve-only -o $(dir $@) $(OUT_FILES)
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --simplify -o $(dir $@) $(OUT_FILES)

$(OUT_FILES): $(OUTPUT)/%.dk: $(INPUT)/%.dk
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --elab-only -o $(dir $@) $<

$(OUT_O_FILES): $(OUTPUT)/%.dko: $(OUTPUT)/%.dk $(OUTPUT)/%.dep $(OUTPUT)/%_univ.dk
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --check-only -o $(dir $@) $<

$(IN_DEP_FILES): $(INPUT)/%.dep: $(INPUT)/%.dk
	$(Q)$(DKDEP) -I $(INPUT) $< > $@

$(OUT_DEP_FILES): $(OUTPUT)/%.dep: $(INPUT)/%.dep
	$(Q)cat $< | sed 's:$(INPUT)/:$(OUTPUT)/:g' > $@

.PHONY: cic.dko
cic.dko:

ifneq ($(MAKECMDGOALS), clean distclean)
-include $(OUT_DEP_FILES)
endif

.PHONY: debug
debug:
	$(Q)echo $(OUT_O_FILES)

.PHONY: clean
clean:
	$(Q)rm $(OUTPUT)/*

.PHONY: distclean
distclean: clean
	$(Q)rm $(INPUT)/*
