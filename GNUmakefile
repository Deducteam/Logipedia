EKSPORTI = _build/install/default/bin/eksporti
DK2JSON = _build/install/default/bin/dk2json

.PHONY: all
all: bin

bin: eksporti dk2json

.PHONY: $(EKSPORTI) $(DK2JSON)
$(EKSPORTI):
	@dune build
$(DK2JSON):
	@dune build

eksporti: $(EKSPORTI)
	@ln -sf $< eksporti

dk2json: $(DK2JSON)
	@ln -sf $< $@

.PHONY: install
install:
	@dune install

.PHONY: doc
doc:
	dune build @doc
	mkdocs build

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
	@$(RM) -f *.lpdb
	@$(RM) -f .*.lpdb
	@$(RM) $(DK2JSON)
	@$(RM) $(EKSPORTI)

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
	@-$(RM) -r /tmp/fermat
