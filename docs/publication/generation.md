# Generating the website

## TL;DR
```sh
logigen -i <input> -o <output> -p <pretty-printer>
```

Once all the json files are generated, the website can be created. To
do so, the `logigen` program in python must be invoked.

Assuming the `json` files are in `export/json` and the website is to
be in `web`,
```sh
logigen -i export/json -o json
```
