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

On the long run, we plan also to support other logics than STTfa, but no plan has been made so far.

# Compiling the project

To compile the project, you need to install the last version of Dedukti [https://github.com/Deducteam/Dedukti](https://github.com/Deducteam/Dedukti). The only dependency needed is `mongo` that you can install with `opam`. Then you can compile the project with

``` bash
make main
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
