#!/usr/bin/env bash

usage="Download Logipedia packages
Usage: $(basename $0) -p PKG -t THY"

dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
source "${dir}/lib.sh"

while getopts 'p:t:h' arg
do
    case "$arg" in
        p) pkg="$OPTARG" ;;
        t) thy="$OPTARG" ;;
	h) echo "$usage"
           exit 0
           ;;
    esac
done

check_req_args "$pkg" "$thy"

rootdl="http://www.lsv.fr/~hondet/logipedia/"
ext=".tar.bz2"
if [[ ! -d "${dkimp}/${thy}" ]]
then
    curl "${rootdl}${thy}${ext}" | tar xj -C "$dkimp"
fi
