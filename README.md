# SharingAnArithmeticLibrary
This repository presents an arithmetic library that is shared between several proof systems. For the moment, the proof systems supported are:
- Coq
- Matita
- OpenTheory

If you want to export this library to your favorite system, please send me a mail to franth2@gmail.com. All contributions are welcome!

# Compiling the project

To compile the project, you need to install Dedukti (version 2.5.1) first that can be found on opam. Then you can compile the project with

``` bash
make main
```

To export the library you can use the command

``` bash
make <system>
```

where `<system>` can be either *coq*, *matita* or *opentheory*. This will create a file *final* in a subdirectory of your system. This file can be checked using the command

``` bash
make test-<system>
```

# Known issue

We don't use the functor mechanism of Coq for the moment because it is heavy to instantiate and hard to use. Moreover, the way the project is build now would introduce a lot of code duplication. One can check the functor output by cloning this branch on the Dedukti project: https://github.com/Deducteam/Dedukti/tree/860965ad1559e374d0451d165a6702e1691b5c3b .
