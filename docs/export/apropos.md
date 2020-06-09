# Exporting proofs from Logipedia to systems

Exporting proofs in Logipedia requires first to translate your proofs in a very
weak logic called STTfa.  Once expressed in this logic, a proof can be exported
to any system supported by Logipedia.

For the web interface, Logipedia is also able to export proofs into a JSON
format.  An other component (namely _Logigen_) is then able to tranfsorm a JSON
file into several web pages.

Translating proofs from one logic to another is quite complicated and requires a
lot of Dedukti knowledge. Currently, only the translation from HOL to STTfa has
been fully automatized. On the short run, it will be possible to have a partial
translation from Matita to STTfa also.

If you are interested in creating a new (partial) translation, please send an
email to <dedukti-dev@inria.fr>!

On the long run, we plan also to support other logics than STTfa, but no
concrete plan has been made so far.

## Supported systems for export
- Coq
- Lean
- Matita
- OpenTheory
- HolLight
- PVS
