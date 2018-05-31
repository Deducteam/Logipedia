#!/bin/sh

if [ $# -eq 0 ]
  then
    echo "Aucun fichier mis en arguments"

  else
    cd ..
    ../main.native -I ../theories/ -I ../library/ --export sttfa ../library/connectives.dk
    ../main.native -I ../theories/ -I ../library/ --export sttfa ../library/logic.dk
    ../main.native -I ../theories/ -I ../library/ --export sttfa ../library/relations.dk
    ../main.native -I ../theories/ -I ../library/ --export sttfa ../library/leibniz.dk
    ../main.native -I ../theories/ -I ../library/ --export sttfa ../library/bool.dk
    ../main.native -I ../theories/ -I ../library/ --export sttfa ../library/div_mod.dk

    make library/$1
    cd bdd
    mongo < noDoublon.js
fi
