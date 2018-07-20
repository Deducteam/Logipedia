# Logipedia
This repository presents an arithmetic library that is shared between several proof systems. For the moment, the proofs can be exported to the following systems:

- Coq
- Matita
- OpenTheory
- Lean
- PVS

If you want to export this library to your favorite system, please send me a mail to francois.thire@inria.fr. All contributions are welcome!

# Compiling the project

To compile the project, you need to install the last version of Dedukti [https://github.com/Deducteam/Dedukti](https://github.com/Deducteam/Dedukti). Then you can compile the project with

``` bash
make main
```

To export the library you can use the command

``` bash
make <system>
```

where `<system>` can be either *coq*, *matita*. *lean* or *pvs*. For *opentheory*, you can compile any file in the */library* directory one by one.

# Known issue

We don't use the functor mechanism of Coq for the moment because it is heavy to instantiate and hard to use. Moreover, the way the project is build now would introduce a lot of code duplication. One can check the functor output by cloning this branch on the Dedukti project: https://github.com/Deducteam/Dedukti/tree/860965ad1559e374d0451d165a6702e1691b5c3b .
