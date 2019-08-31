DKMETA  = dkmeta

PACKAGE = opentheory_stdlib/packages
THEORY  = hol

FROM    = ../../import/dedukti/$(THEORY)/$(PACKAGE)
TO      = ../../import/dedukti/sttfa/opentheory_stdlib

FROM_FILES = $(wildcard $(FROM)/*.dk)
TO_FILES   = $(addprefix $(TO)/, $(addsuffix .dk, $(basename $(notdir $(wildcard $(FROM)/*.dk)))))

.PHONY: all
all: default

.PHONY: default
default: $(FILES) $(TO) $(TO_FILES)
	echo $(TO_FILES)
	cp sttfa_hol_axioms.dk $(TO)
	cp leibniz.dk $(TO)

$(TO):
	mkdir -p $(TO)

$(TO)/%.dk: $(FROM)/%.dk
	$(DKMETA) -m hol_sttfa.dk -m sttfa_inv_product.dk --encoding prod $< > $@
