# Logipedia

Logipedia is a project that aims to share formal proofs between several systems.
The website displaying all available proofs can be reached at
[http://logipedia.science]. The available systems are (with E for export and I
for import)
- Coq (I/E)
- Lean (E)
- Matita (I/E)
- OpenTheory (I/E)
- HolLight (I/E)
- PVS (E)

## Documentation
The source of the project documentation can be found in [`docs`](docs). The
documentation can be browsed at [https://deducteam.github.io/Logipedia] or it
can be built locally and then browsed using [`mkdocs`](https://www.mkdocs.org/) with

``` bash
mkdocs build
mkdocs serve
```
then head to [127.0.0.1:8000].

The code is also documented using `dune`, use `make doc` to build it. It will
be available in `_build/default/doc/`.

## Contributing to Logipedia
To contribute to the API, simply fork the repository and make a pull request. To
work on new translations, send us an email.

## Maintainers
Logipedia:
- François Thiré <francois.thire@lsv.fr>
- Gabriel Hondet <gabriel.hondet@lsv.fr>
- Emilie Grienenberger <emilie.grienenberger@lsv.fr>

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
