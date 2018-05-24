#!/bin/sh

if [ $# -eq 0 ]
  then
    echo "Aucun fichier mis en arguments"

  else
    cd ..
    make library/$1
    cd bdd
    mongo < noDoublon.js
fi
