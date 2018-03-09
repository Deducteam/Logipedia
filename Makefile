default: main

all: main

main:
	ocamlbuild -package dedukti.parser src/main.native

clean:
	ocamlbuild -clean

.PHONY: main
