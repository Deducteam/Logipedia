#!/usr/bin/env bash
# Any script can source this one to init environment
set -euf -o pipefail

dir="$(cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd)"
root="$(realpath "${dir}/../")"
dkimp="${root}/import/dedukti"
mkdir -p "$dkimp"

# Exit with a message
exit_with () {
    echo "${1:-'Unknown error'}" 1>&2
    exit 1
}

# checks that all values passed as parameters are not empty
check_req_args () {
    if [[ "$#" == "0" ]]
    then
        exit 1
    fi
    while (( "$#" ))
    do
        if [[ -z "$1" ]]
        then
            exit_with "Missing argument"
        else
            shift
        fi
    done
}

# Call with theory as first arg and package as snd to set up environment
setup () {
    check_req_args "$1" "$2"
    "${root}/utils/download.sh" -t "$1" -p "$2"
}
