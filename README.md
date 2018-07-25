# Logipedia
This repository presents an arithmetic library that is shared between several proof systems. For the moment, the proofs can be exported to the following systems:

- Coq
- Matita
- OpenTheory
- Lean
- PVS

If you want to export this library to your favorite system, please send me a mail to francois.thire@inria.fr. All contributions are welcome!

# Compiling the project

To compile the project, you need to install the last version of Dedukti [https://github.com/Deducteam/Dedukti](https://github.com/Deducteam/Dedukti). The only dependency needed is `mongo` that you can install with `opam`. Then you can compile the project with

``` bash
make main
```

To export the library you can use the command

``` bash
make <system>
```

where `<system>` can be either *coq*, *matita*. *lean* or *pvs*. For *opentheory*, you can compile any file in the */library* directory one by one.

# Exporting the proofs on MongoDB

This process requires first to have a MongoDB server. You can install one with the following command:
`sudo apt install mongodb`

Then you need to run a server with the following command:
`mongo`

To export your theorems, you just need to use the option `--export-bdd`. Using the command `make`, you can do it by setting the environment variable `BDD`. For example:
`make BDD=--export-bdd library/fermat.art`

# Maintainers

- Lean/Coq/Matita/OpenTheory : François Thiré
- PVS : Gilles Dowek

# Known issue

- We don't use the functor mechanism of Coq for the moment because it is heavy to instantiate and hard to use.
- OpenTheory output comsumes a lot of memory (about 4Go). This is due to a memoization problem on our side. This should be fixed in a future version.
