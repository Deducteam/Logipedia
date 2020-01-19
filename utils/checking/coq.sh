#!/usr/bin/env bash

usage="Coq checker: $0 -d <coqdir>"

while getopts 'hd:' arg; do
    case "$arg" in
        d)
            coqdir=$OPTARG
            ;;
        h)
            echo "$usage"
            exit 0
            ;;
    esac
done

if [[ -z ${coqdir:-''} ]]
then
    echo -e 'Directory containing .v files not specified'
    echo -e "$usage"
    exit 1
fi
# Sanitize names
rename 's:-:_:g' "${coqdir}/*.v"
rename 's:\.:_:g' "${coqdir}/*.v"
cd "$coqdir"
ls *.v > _CoqProject
coq_makefile -f _CoqProject -o Makefile
make
cd -
