#!/usr/bin/env bash
# TODO add the parsing of Dedukti options
usage="Check dk files of a Logipedia library
Usage: $(basename $0) -t THY -p PKG -- [Dk options]"

source "${0%/*}/../lib.sh"
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

setup "$thy" "$pkg"

src="${dkimp}/${thy}/${pkg}/"
thdir="${root}/theories/${thy}"
fls="$(dkdep -s -I ${thdir} -I ${src} ${thdir}/*.dk ${src}/*.dk)"
for f in ${fls}
do
    dkcheck -I "${thdir}" -I "${src}" -e "$f"
done
