#!/usr/bin/env bash

DIR="${0%/*}"
usage="OpenTheory checker: $0 -d DIR -I DIR -p PKG
\td\tDirectory with files to check
\tI\tDirectory with Dedukti files
\tp\tName of library"

while getopts 'hI:p:d:' arg; do
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
# TODO
