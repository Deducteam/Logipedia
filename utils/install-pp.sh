#!/usr/bin/env bash

### Install script for the logipedia pretty printer. Installs a scheme pretty
### printer.

_logipp="logipp-latex"

### Check for dependencies

if [[ -e "$(which ${_logipp})" ]]
then
    exit 0
fi

## Check for guile
if [[ -z "$(which guile)" ]]
then
    echo "Guile scheme required"
    echo "I won't install it for you"
    exit 1
fi

## guile json
guile -c '(use-modules (json))'
if [[ "$?" -ne 0 ]]
then
    echo "Guile json required"
    read -p "Install guile-json? [y/N]" answer
    if [[ ! "$answer" -eq "y" ]]
    then
        exit 1
    fi
fi

_wd=$(pwd)

### Installing guile json
echo "Installing guile json..."
tmpd="$(mktemp -d)"
repo="https://git.savannah.nongnu.org/git/guile-json.git"
cd "$tmpd"
git clone "$repo" --branch 3.1.0 --quiet .
autoreconf -if
# FIXME get prefix from guile %site-dir?
./configure --prefix=/usr
make
sudo make install
cd -
rm -rf "$tmpd"

### Installing the pretty printer
echo "Installing the pretty printer"
tmpd="$(mktemp -d)"
repo="https://github.com/gabrielhdt/logippedia.git"
cd "$tmpd"
git clone "$repo" --quiet .
cd 'scheme'
sudo make install
cd "$_wd"
rm -rf "$tmpd"

if [[ -z "$(which ${_logipp})" ]]
then
    echo "Installation error"
    echo "You might want to check your paths"
    exit 1
fi
