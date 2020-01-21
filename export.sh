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

# Download library
cd "import/dedukti"
if [[ ! -d "$thy" ]]
then
    curl "http://www.lsv.fr/~hondet/logipedia/${thy}.tar.bz2" | tar xj
fi
cd -

out="export/${exp}"
thdir="theories/${thy}"
srcdir="import/dedukti/${thy}/${pkg}"

if [[ "$exp" == "json" ]]
then
    middleware=${mid:-"$thy"}
    ./dk2json -m "$middleware" -o "$out" -J "$out"\
              -I "$thdir" -I "$srcdir" -d "$srcdir"\
              --dkopts "'${dkopts:-''}'"
else
    ./logipedia "$exp" -I "$thdir" -I "$srcdir" -o "$out" -d "$srcdir"
fi
