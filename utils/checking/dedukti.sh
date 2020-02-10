#!/usr/bin/env bash
usage="Check dk files of a Logipedia library
Usage: $(basename "$0") -t THY -p PKG -k DKOPTS"

dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
source "${dir}/lib.sh"

while getopts 'p:t:k:h' arg
do
    case "$arg" in
        p) pkg="$OPTARG" ;;
        t) thy="$OPTARG" ;;
	k) dkopts="$OPTARG" ;;
        h) echo "$usage"
           exit 0
           ;;
    esac
done

setup "$thy" "$pkg"

src="${dkimp}/${thy}/${pkg}/"
thdir="${root}/theories/${thy}"
fls="$(dkdep -s -I "${thdir}" -I "${src}" ${thdir}/*.dk ${src}/*.dk)"
for f in ${fls}
do
    dkcheck -I "${thdir}" -I "${src}" -e "$f" -k "${dkopts:-''}"
done
