#!/usr/bin/env bash
usage="Dk checker: $0 -p PKG -t THY -- [Dk options]"

set -x

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

src="${dkimp}/${thy}/${pkg}/"
# Download package and theory if it doesn't exist
if [[ ! -d "${dkimp}/${thy}" ]]
then
    curl "http://www.lsv.fr/~hondet/logipedia/${thy}.tar.bz2" | tar xj
fi

thdir="${root}/theories/${thy}"
fls="$(dkdep -s -I ${thdir} -I ${src} ${thdir}/*.dk ${src}/*.dk)"
for f in ${fls}
do
    dkcheck -I "${thdir}" -I "${src}" -e "$f"
done
