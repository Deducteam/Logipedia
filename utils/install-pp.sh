#!/usr/bin/env bash

### Install script for the logipedia pretty printer. Installs a scheme pretty
### printer.

_logipp="logipp-latex"

### Check for dependencies

if [[ -e "$(command -v ${_logipp})" ]]
then
    exit 0
fi

## Check for guile
if [[ -z "$(command -v guile)" ]]
then
    echo "Guile scheme required"
    echo "I won't install it for you"
    exit 1
fi

## guile json
guile -c '(use-modules (json))'
if [[ "$(guile -c '(use-modules (json))')" -ne 0 ]]
then
    echo "Guile json required"
    read -rp "Install guile-json (from sources)? [y/N]" answer
    if [[ ! "$answer" -eq 'y' ]]
    then
        exit 1
    fi
fi

_wd=$(pwd)

### Installing guile json
echo "Installing guile json..."
tmpd="$(mktemp -d)"
repo="https://git.savannah.nongnu.org/git/guile-json.git"
(cd "$tmpd" || exit
 git clone "$repo" --branch 3.1.0 --quiet . > /dev/null
 autoreconf -if > /dev/null
 # FIXME get prefix from guile %site-dir?
 ./configure --prefix=/usr > /dev/null
 make
 sudo make install)
rm -rf "$tmpd"

### Installing the pretty printer
echo "Installing the pretty printer"
tmpd="$(mktemp -d)"
repo="https://github.com/gabrielhdt/logippedia.git"
(cd "$tmpd" || exit
 git clone "$repo" --quiet . > /dev/null
 cd 'scheme' || exit
 sudo make install)
rm -rf "$tmpd"

if [[ -z "$(command -v ${_logipp})" ]]
then
    echo "Installation error"
    echo "You might want to check your paths"
    exit 1
else
    echo "Installation succeeded: $(command -v ${_logipp})"
    exit 0
fi
