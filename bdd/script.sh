#!/bin/sh
cd ..
make
for fichier in library/*.dk
do
  ./main.native -I theories/ -I library/ --export sttfa $fichier
done

if [ $# -eq 0 ]
  then
    make library/fermat.ma
    cd bdd
    mongo < noDoublon.js
  else
    make library/$1
    cd bdd
    mongo < noDoublon.js
fi
