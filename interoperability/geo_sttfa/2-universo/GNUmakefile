Q = @

INPUT=input
OUTPUTRAW=output_raw
OUTPUT=output

IN_FILES=$(wildcard $(INPUT)/*.dk)
LIB=$(notdir $(basename $(IN_FILES)))

UNIV_FILES=$(LIB:%=$(OUTPUTRAW)/%_univ.dk)
UNIV_O_FILES=$(LIB:%=$(OUTPUTRAW)/%_univ.dko)

CSTR_FILES=$(LIB:%=$(OUTPUTRAW)/%_cstr.dk)
SOL_FILES=$(LIB:%=$(OUTPUTRAW)/%_sol.dk)

OUT_FILES=$(LIB:%=$(OUTPUTRAW)/%.dk)
OUT_O_FILES=$(LIB:%=$(OUTPUTRAW)/%.dko)

OUT_NF_FILES=$(LIB:%=$(OUTPUT)/%.dk)

IN_DEP_FILES=$(LIB:%=$(INPUT)/%.dep)
OUT_DEP_FILES=$(LIB:%=$(OUTPUTRAW)/%.dep)

THEORY=Coq.dk
CONFIG=config.dk

UNIVERSO_OPTIONS=\
	$(DEBUG) \
	--theory $(THEORY) \
	--config $(CONFIG)

.PHONY: default
default: $(OUT_NF_FILES)

$(OUTPUTRAW):
	mkdir -p $(OUTPUTRAW)

$(UNIV_FILES): $(OUTPUTRAW)/%_univ.dk: $(OUTPUTRAW)/%.dk

$(UNIV_O_FILES): $(OUTPUTRAW)/%_univ.dko: $(OUTPUTRAW)/%_univ.dk
	$(Q)$(DKCHECK) -e -I $(dir $(THEORY)) $<

$(CSTR_FILES): $(OUTPUTRAW)/%_cstr.dk: $(OUTPUTRAW)/%.dko


.PHONY: solve
solve:
	$(Q)time -o /tmp/solved_time $(UNIVERSO) $(UNIVERSO_OPTIONS) --solve-only -o $(OUTPUTRAW)  $(OUT_FILES)

$(SOL_FILES): $(CSTR_FILES) solve

$(OUT_NF_FILES): $(OUTPUT)/%.dk: $(OUTPUTRAW)/%_sol.dk
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --simplify $(dir $@) -o $(OUTPUTRAW) $(OUTPUTRAW)/$*.dk

$(OUT_FILES): $(OUTPUTRAW)/%.dk: $(INPUT)/%.dk
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --elab-only -o $(OUTPUTRAW) $<

$(OUT_O_FILES): $(OUTPUTRAW)/%.dko: $(OUTPUTRAW)/%.dk $(OUTPUTRAW)/%.dep $(OUTPUTRAW)/%_univ.dk
	$(Q)$(UNIVERSO) $(UNIVERSO_OPTIONS) --check-only -o $(OUTPUTRAW) $<

$(IN_DEP_FILES): $(INPUT)/%.dep: $(INPUT)/%.dk
	$(Q)$(DKDEP) -I $(INPUT) $< > $@

$(OUT_DEP_FILES): $(OUTPUTRAW)/%.dep: $(INPUT)/%.dep
	$(Q)cat $< | sed 's:$(INPUT)/:$(OUTPUTRAW)/:g' > $@

Coq.dko:
	dkcheck -e Coq.dk

ifneq ($(MAKECMDGOALS), clean distclean)
-include $(OUT_DEP_FILES)
endif

.PHONY: debug
debug:
	$(Q)echo $(OUT_O_FILES)

.PHONY: clean
clean:
	$(Q)rm $(OUTPUTRAW)/* 2> /dev/null || true
	$(Q)rm $(OUTPUT)/* 2> /dev/null || true

.PHONY: distclean
distclean: clean
	$(Q)rm $(INPUT)/* 2> /dev/null || true
