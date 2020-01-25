#!/usr/bin/env bash

progname="$(basename $0)"
usage="Download Logipedia packages
Usage: $progname -p PKG -t THY"

source "${0%/*}/lib.sh"

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

check_not_null "$pkg" "$thy" || exit_with "$LINENO: Missing argument"

rootdl="http://www.lsv.fr/~hondet/logipedia/"
ext=".tar.bz2"
if [[ ! -d "${dkimp}/${thy}" ]]
then
    curl "${rootdl}${thy}${ext}" | tar xj -C "$dkimp"
fi
