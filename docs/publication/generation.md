---
layout: default
title: Generating the website
---
# TL;DR
```sh
logigen -i <input> -o <output> -p <pretty-printer>
```

# The HTML files
Once all the json files are generated, the website can be created. To
do so, the `logigen` program in python must be invoked.

Assuming the `json` files are in `export/json` and the website is to
be in `web`,
```sh
logigen -i export/json -o json
```

# Pretty printing
The previous procedure yields HTML files with extracts from the JSON
files for the mathematical content. These extracts can be processed
during the conversion by an external program to yield pleasant
formulae.

This program must accept the JSON on its standard input and output the
processed result on its standard output.

Assuming the prettyfier is called `pretty`, using the
`--pretty-printer` option with `logigen` triggers the aforementioned
processing. Arguments are passed to `pretty` via the `pp-extra`
option (assuming `$args` contains the arguments):
```sh
logigen -i export/json -o json --pretty-printer pretty --pp-extra "$args"
```

Such a pretty printer has been developed in Guile Scheme at
<https://github.com/gabrielhdt/LogiPPedia>. Refer to its manual page
for more information.
