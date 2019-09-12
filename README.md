# Logipedia

Logipedia is a project that aims to share formal proofs between several systems. The systems supported are (E for export and I for import):
- Coq (E)
- Lean (E)
- Matita (I/E)
- OpenTheory (I/E)
- PVS (E)

The project is based upon the logical framework [https://github.com/Deducteam/Dedukti](Dedukti). The project has also an interface via the [http;//logipedia.science](Logipedia) website.

# Importing proofs

Importhing proofs in Logipedia require first to import your proofs in Dedukti. Currently, this is possible only for Matita via [https://github.com/Deducteam/matita](Krajono) and OpenTheory via [https://github.com/Deducteam/Holide](Holide). A lot of efforts is made to support Coq via [https://github.com/Deducteam/CoqInE](CoqInE).

# Exporting proofs

Exporting proofs in Logipedia requires first to translate your proofs in a very weak logic called STTfa. Once your proofs can be expressed in this logic, we can export them to Coq, Lean, Matita, OpenTheory and PVS.

Translating proofs from one logic to another is quite complicated and requires a lot of Dedukti knowledge. Currently, only the translation from HOL to STTfa has ben fully automatized. On the short run, it will be possible to have a partial translation from Matita to STTfa also.

If you are interested in creating a new (partial) translation, please send us an email!

On the long run, we plan also to support other logics than STTfa, but no concrete plan has been made so far.

# Compiling the project

To compile the project, you need to install the last version of Dedukti (master branch) [https://github.com/Deducteam/Dedukti](https://github.com/Deducteam/Dedukti).
Then compile with

``` bash
make
```

To export the proofs to a local Mongo DB and a set of flat files, run

``` bash
make web
```

To export the library you can use the command

``` bash
make <system>
```

where `<system>` can be either *coq*, *matita*. *lean* or *pvs* or *opentheory*. You can also compile all the files one:

``` bash
make export/<system>/<file>
```
`file` should be in `import/dedukti/<LOGIC>/<PACKAGE>` where `PACKAGE` and `LOGIC` are Makefile variables. By default `PACKAGE=arith_fermat` and `LOGIC=sttfa`.

# Maintainers

Logipedia:
- François Thiré <francois.thire@lsv.fr>
- Gabriel Hondet <gabriel.hondet@lsv.fr>

Export systems:
- Lean/Coq/Matita/OpenTheory : François Thiré <francois.thire@lsv.fr>
- PVS : Gilles Dowek <gilles.dowek@lsv.fr>

Translations:
- HOL to STTfa : Chantal Keller <Chantal.Keller@lri.fr>
# Known issue

- We don't do any concept alignement: every proofs comme in an axiomatized way. You need then to align the concepts of the library from Logipedia to the ones of your system. But if you are interested to try do something, send us an email.
- OpenTheory output comsumes a lot of memory (about 4Go). This is due to a memoization problem on our side. We hope to fix this in a future version.
- For the website, we generate all the files first. It won't scale up, but this is to avoid a latency issue when the user wants to get the file.
