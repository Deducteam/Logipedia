---
layout: default
title: Adding imported libraries to Logipedia
---
A library in Logipedia is made of two elements:

- the encoding of the theory in which the library is expressed;
- the library itself;
- some Ocaml code to interpret faithfully the elements encoded so that
  the original structure remains.

**Some Ocaml?**
The Ocaml code is used to, for instance, distinguish a lemma from a
theorem. More generally, the structure of the input library can be
found back thanks to term analysis.

When supplying a library, one may provide a "middleware" with it:

- develop a module implementing the `Middleware.S` interface;
- add a binding in the `Middleware` module, see for instance the
  `Middleware.MidSttfa` module.

The new middleware will then be available for export to `json`.
