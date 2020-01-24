#!/bin/bash

usage="Usage: $0 -e EXP -p PKG -t THY [-m MID] [-k DKOPT]"
while getopts 'e:p:t:d:m:k:h' arg
do
    case "$arg" in
        m) mid="$OPTARG" ;;
        e) exp="$OPTARG" ;;
        t) thy="$OPTARG" ;;
        p) pkg="$OPTARG" ;;
        k) dkopts="$OPTARG" ;;
        h) echo "$usage"
           exit 0
           ;;
    esac
done

if [[ -z ${exp:-''} ]]
then
    echo 'Export mode not specified'
    echo "$usage"
    exit 1
fi

$(utils/download.sh -p "$pkg" -t "$thy")

out="export/${exp}"
thdir="theories/${thy}"
srcdir="import/dedukti/${thy}/${pkg}"
middleware=${mid:-"$thy"}

make
if [[ "$exp" == "json" ]]
then
    ./dk2json -m "$middleware" -o "$out" -J "$out"\
              -I "$thdir" -I "$srcdir" -d "$srcdir"\
              --dkopts "'${dkopts:-''}'"\
              --hollight "export/hollight"\
              --pvs "export/pvs"\
              --lean "export/lean"\
              --coq "export/coq"\
              --matita "export/matita"
else
    ./eksporti "$exp" -I "$thdir" -I "$srcdir" -o "$out"\
               -d "$srcdir" -m "$middleware"
fi
