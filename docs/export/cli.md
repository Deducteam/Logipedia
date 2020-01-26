# Export command line interface

## Exporting from STTfa files
*NOTE* currently, only the export from STTfa files is available.

Exporting a library requires
- a logic encoded in one or more dedukti files, those files are in `theories/sttfa`;
- dedukti files of the library encoded in the STTfa logic in `import/sttfa/lib`

### TL;DR
To export the package `arith_fermat` using theory `sttfa` to PVS,
```bash
utils/export.sh -e pvs -p arith_fermat -t sttfa
```
### Options
- `-m` specify the middleware used
- `-k` specify other Dedukti options

## For developers
Bash scripts allow to preset many options. `export.sh` uses the binary
`eksporti` which comes with many more options,

- `-I` add a folder containing Dedukti source files,
- `-d` add a directory containing Dedukti files to export,
- `--debug N` with `0 <= n <= 7` enables debugging, the lower the quieter,
- `--dkopts OPTS` pass options to Dedukti to produce `dko` files,
- `-o` set output directory

*Note* that each system may come with its own options;
to know more about the export options of any system `sys`:
```bash
dune exec -- eksporti sys --help
```
and to list available systems,
```bash
dune exec -- eksporti --help
```
