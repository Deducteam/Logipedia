#!/usr/bin/env bash
# Any script can source this one to init environment

root="$(realpath ${0%/*}/../)"
dkimp="${root}/import/dedukti"
mkdir -p "$dkimp"

# checks that all values passed as parameters are not empty
check_not_null () {
    if [[ "$#" == "0" ]]
    then
        exit 1
    fi
    while (( "$#" ))
    do
        if [[ -z "$1" ]]
        then
            return 1
        else
            shift
        fi
    done
    return 0
}

exit_with () {
    echo "${0:-'Unknown error'}" 1>&2
    exit 1
}
