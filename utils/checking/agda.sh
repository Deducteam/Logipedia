#!/bin/bash

usage="Agda checker: $0 -d <dir>"

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
(cd ${indir} ; find . -type f -name "*.agda" -exec agda -W noMissingDefinitions --type-in-type --prop {} \;)
