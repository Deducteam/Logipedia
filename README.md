# Logipedia

Logipedia is a project that aims to share formal proofs between several systems.
The systems supported are (E for export and I for import):
- Coq (E)
- Lean (E)
- Matita (I/E)
- OpenTheory (I/E)
- PVS (E)

The project is based upon the logical framework
[Dedukti](https://github.com/Deducteam/Dedukti). The project has also an
interface via the [Logipedia website](https://logipedia.science).

## Importing proofs

Importing proofs in Logipedia boils down to translating proofs into Dedukti.
Currently, this is possible only for Matita via
[Krajono](https://github.com/Deducteam/matita) and OpenTheory via
[Holide](https://github.com/Deducteam/Holide). A lot of efforts is made to
support Coq via [CoqInE](https://github.com/Deducteam/CoqInE).

## Exporting proofs

Exporting proofs in Logipedia requires first to translate your proofs in a very
weak logic called STTfa.  Once expressed in this logic, a proof can be exported
to any system supported by Logipedia.

For the web interface, Logipedia is also able to export proofs into a JSON
format.  An other component (namely _Logigen_) is then able to tranfsorm a JSON
file into several web pages.

Translating proofs from one logic to another is quite complicated and requires a
lot of Dedukti knowledge. Currently, only the translation from HOL to STTfa has
ben fully automatized. On the short run, it will be possible to have a partial
translation from Matita to STTfa also.

If you are interested in creating a new (partial) translation, please send us an
email!

On the long run, we plan also to support other logics than STTfa, but no
concrete plan has been made so far.

## Compiling the project

To compile the project, you need to install the latest version of Dedukti
[master branch](https://github.com/Deducteam/Dedukti/tree/master). Then compile
with

```shell
make
```

## Makefile interface
Since Logipedia deals with many files, calls to the binary are mainly managed
through the makefile and a specific hierarchy of files.

### Static hierarchy
- `import/dedukti` contains dedukti source files;
- `theory` contains dedukti encodings of logics used by dedukti source files

### Exporting to a system
Exported files are placed in `<expdir>/<system>`. To create all files for a
given system,
``` bash
make THEORY=<theory> PKG=<pkg> EXPDIR=<expdir> <system>
```
where
- `<pkg>` is the folder in `import/dedukti/<theory>` containing the dedukti
  files to export;
- `<theory>` is the name of the directory in the `theory` directory used by the
  files in the package.
- `<expdir>` is a directory containing the exported files.
- `<system>` can be either *coq*, *matita*, *lean*, *pvs* or *opentheory*.

### Examples

``` bash
make json THEORY=ctpicef PKG=std DKFLAGS=--eta
make json THEORY=sttfa PKG=arith_fermat
```

## Maintainers

Logipedia:
- François Thiré <francois.thire@lsv.fr>
- Gabriel Hondet <gabriel.hondet@lsv.fr>
- Émilie Grienenberger <emilie.grienenberger@lsv.fr>

Export systems:
- Lean/Coq/Matita/OpenTheory : François Thiré <francois.thire@lsv.fr>
- PVS : Gilles Dowek <gilles.dowek@lsv.fr>

Translations:
- HOL to STTfa : Chantal Keller <Chantal.Keller@lri.fr>

## Known issues
- We don't do any concept alignement: every proofs comme in an axiomatized way.
  You need then to align the concepts of the library from Logipedia to the ones
  of your system. But if you are interested to try do something, send us an
  email.
- OpenTheory output comsumes a lot of memory (about 4Go). This is due to a
  memoization problem on our side. We hope to fix this in a future version.
- For the website, we generate all the files first. It won't scale up, but this
  is to avoid a latency issue when the user wants to get the file.
