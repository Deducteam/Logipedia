# Logipedia

Logipedia is a project that aims to share formal proofs between several systems.
The website displaying all available proofs can be reached at
http://logipedia.science.

## Installation
### Dependencies
- Dedukti commit `5990bc6`
  ```shell
  git clone https://github.com/deducteam/dedukti.git
  git checkout 5990bc6
  make install
  ```
- `ppx_deriving_yojson`
- `dune`

### Build & install

``` bash
make install
```

## Documentation
### Project documentation
The source of the project documentation can be found in [`docs`](docs). The
documentation can be browsed at https://deducteam.github.io/Logipedia or it
can be built locally and then browsed using [`mkdocs`](https://www.mkdocs.org/) 
with

``` bash
mkdocs build
mkdocs serve
```
then head to http://127.0.0.1:8000.

### API documentation
The code is also documented with `dune`, use `make doc` to build it. It will
be available in `_build/default/doc/`.

## Contributing to Logipedia
To contribute to the API, simply fork the repository and make a pull request. To
work on new translations, send us an email.
