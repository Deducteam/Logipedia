#!/bin/bash

usage="Coq checker: $0 -d <dir>"

while getopts 'hd:' arg; do
    case "$arg" in
        d)
            indir=$OPTARG
            ;;
        h)
            echo "$usage"
            exit 0
            ;;
    esac
done

if [[ -z ${indir:-''} ]]
then
    echo -e 'Directory containing source files not specified'
    echo -e "$usage"
    exit 1
fi
# Sanitize names
rename 's:-:_:g' "${indir}/*.v"
rename 's:\.:_:g' "${indir}/*.v" 2> /dev/null
(cd "$indir" || exit
 ls ./*.v > _CoqProject
 coq_makefile -f _CoqProject -o Makefile
 make)
