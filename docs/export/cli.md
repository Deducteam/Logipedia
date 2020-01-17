# Export command line interface

## Exporting from STTfa files
*NOTE* currently, only the export from STTfa files is available.

Exporting a library requires
- a logic encoded in one or more dedukti files, those files are in `theories/sttfa`;
- dedukti files of the library encoded in the STTfa logic in `import/sttfa/lib`

### TL;DR
To export to the system `sys` into directory `export/sys`,
```sh
./logipedia sys -I import/sttfa/lib -I theories/sttfa -d
import/sttfa/lib -o export/sys
```
### Options
- `-I` add a folder containing Dedukti files that are needed
- `-d` add a directory containing Dedukti files to convert
- `-o` output directory
- `--debug n` print debugging message with `0 <= n <= 7`, the lower
  the quieter

*Note* that with STTfa, the available exports are Coq, HolLight, Lean,
Matita, OpenTheory and PVS. Each system may come with its own options;
to know more:
```sh
./logipedia sys --help
```
