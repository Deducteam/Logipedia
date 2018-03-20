DKCHECK=dkcheck
DKDEP=dkdep
ML  := $(wildcard src/*.ml)
MLI := $(wildcard src/*.mli)
all: main.native

main.native: _build/src/main.native

_build/src/main.native: $(ML) $(MLI)
	ocamlbuild -package dedukti.parser src/main.native

sttfa.dko: theories/sttfa.dk
	$(DKCHECK) -e $^

examples: sttfa.dko main.native
	for f in $(wildcard examples/*.dk); do \
		./main.native $$f ; \
	done;

library: .depend $(wildcard library/*.dko)

library/%.dko: sttfa.dko library/%.dk .depend
	$(DKCHECK) $<

.depend: $(wildcard library/*.dk)
	@$(DKDEP) --ignore $^ -o $@

clean:
	ocamlbuild -clean

.PHONY: main.native clean examples .depend library
