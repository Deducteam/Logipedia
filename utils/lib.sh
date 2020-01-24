#!/usr/bin/env bash

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
