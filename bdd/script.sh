#!/bin/sh
cd ..
make distclean
make
make theories/sttfa.dko
make bdd-dep

if [ $# -eq 0 ]
  then
    make BDD=--export-bdd library/fermat.ma
    make BDD=--export-bdd library/fermat.v
    make BDD=--export-bdd library/fermat.lean
    make BDD=--export-bdd library/fermat.pvs
    cd bdd
    mongo < noDoublon.js
  else
    make BDD=--export-bdd library/$1
    cd bdd
    mongo < noDoublon.js
fi
