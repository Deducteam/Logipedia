#!/usr/bin/env bash
usage="Check dk files of a Logipedia library
Usage: $0 -p PKG -t THY -- [Dk options]"

dir="${0%/*}"
root="$(realpath ${dir}/../../)"
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
    echo -e "Package or theory not given"
    echo "$usage"
    exit 1
fi

$($root/utils/download.sh -p "$pkg" -t "$thy")

src="${dkimp}/${thy}/${pkg}/"
thdir="${root}/theories/${thy}"
fls="$(dkdep -s -I ${thdir} -I ${src} ${thdir}/*.dk ${src}/*.dk)"
for f in ${fls}
do
    dkcheck -I "${thdir}" -I "${src}" -e "$f"
done