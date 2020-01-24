#!/usr/bin/env bash

usage="Download Logipedia packages
Usage: $0 -p PKG -t THY"

dir="${0%/*}"
root="$(realpath ${dir}/../)"
dkimp="${root}/import/dedukti/"

while getopts 'p:t:h' arg
do
    case "$arg" in
        p) pkg="$OPTARG" ;;
        t) thy="$OPTARG" ;;
        h)
            echo "$usage"
            exit 0
            ;;
    esac
done

if [[ -z "$pkg" ]] || [[ -z "$thy" ]]
then
    echo "Package or theory not provided"
    echo ""
    echo "$usage"
    exit 1
fi

rootadd="http://www.lsv.fr/~hondet/logipedia/"
ext=".tar.bz2"
if [[ ! -d "${dkimp}/${thy}" ]]
then
    curl "${rootadd}${thy}${ext}" | tar xj -C "$dkimp"
fi
