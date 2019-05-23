# Logipedia
This repository presents an arithmetic library that is shared between several proof systems. For the moment, the proofs can be exported to the following systems:

- Coq
- Matita
- OpenTheory
- Lean
- PVS

If you want to export this library to your favorite system, please send us an email: gilles.dowek@inria.fr, francois.thire@inria.fr. All contributions are welcome!

# Compiling the project

To compile the project, you need to install the last version of Dedukti (master branch) [https://github.com/Deducteam/Dedukti](https://github.com/Deducteam/Dedukti). The only dependency needed is `mongo` that you can install with `opam`. Currently, this dependency requires a version of OCaml `< 4.06`. However the `mongo` dependency will be dropped in the future. Then you can compile the project with

``` bash
make
```

To export the library you can use the command

``` bash
make <system>
```

where `<system>` can be either *coq*, *matita*. *lean* or *pvs* or *opentheory*. You can also compile all the files one:

``` bash
make library/nat.v
```

to generate coq files for example.

# Exporting the MongoDB Database

This process requires first to have a MongoDB server. You can install one with the following command:
`sudo apt install mongodb`

Then you need to run a server with the following command:
`mongo`

To export the Mongo DB, you just need to invoke the Makefile with the following command:
`make bdd-dep`

In the `bdd` folder, you will find scripts related to the bdd. In particular, since we are using MongoDB, it is better if you drop the old database before updating it.

# Maintainers

- Lean/Coq/Matita/OpenTheory : François Thiré
- PVS : Gilles Dowek

# Known issue

- We don't use the functor mechanism of Coq for the moment because it is heavy to instantiate and hard to use.
- OpenTheory output comsumes a lot of memory (about 4Go). This is due to a memoization problem on our side. This should be fixed in a future version.
- Files are generated all at the same time. In later versions, we would like to generate them one by one.
