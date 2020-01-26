.PHONY: all
all: bin

.PHONY: bin
bin:
	@dune build

.PHONY: install
install:
	@dune install

.PHONY: doc
doc:
	dune build @doc
	mkdocs build

# FIXME logipp-latex definitve?
.PHONY: pp
install_pp:
	utils/install-pp.sh

.PHONY: clean
clean:
	@dune clean
	@$(RM) -f *.lpdb
	@$(RM) -f .*.lpdb

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
