# Ready to use targets

.PHONY: all ctpicef sttfa

all: ctpicef sttfa

ctpicef:
	$(MAKE) json EXPDIR=export/$@ THEORY=ctpicef PKG=std MIDDLEWARE=cic DKFLAGS=--eta
sttfa:
	$(MAKE) json EXPDIR=export/$@ THEORY=sttfa PKG=arith_fermat MIDDLEWARE=sttfa
