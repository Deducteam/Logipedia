#!/bin/bash

dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
source "${dir}/lib.sh"
usage="Usage: $(basename "$0") -e EXP -t THY -p PKG [-m MID] [-k DKOPT]"
while getopts 'e:p:t:m:k:h' arg
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
        *) echo "Invalid option"
           echo "$usage"
           exit 1
           ;;
    esac
done

if [[ -z ${exp:-''} ]]
then
    echo 'Export mode not specified'
    echo "$usage"
    exit 1
fi

setup "$thy" "$pkg"

# Use relative paths for ocaml
out="${root}/export/${exp}"
thdir="${root}/theories/${thy}"
srcdir="${root}/import/dedukti/${thy}/${pkg}"
middleware=${mid:-"$thy"}

(cd "$root" || exit
 if [[ "$exp" == "json" ]]
 then
   dune exec -- dk2json -m "$middleware" -o "$out" -J "$out"\
     -I "$thdir" -I "$srcdir" -d "$srcdir"\
     --dkopts "'${dkopts:-''}'"\
     --hollight "export/hollight"\
     --pvs "export/pvs"\
     --lean "export/lean"\
     --coq "export/coq"\
     --matita "export/matita"\
     --agda "export/agda"
 else
   dune exec -- eksporti "$exp" -I "$thdir" -I "$srcdir" -o "$out"\
     -d "$srcdir" -m "$middleware"
 fi)
