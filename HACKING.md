# Structure

- *theories* contains Dedukti files that define a theory in Dedukti (such as STTforall, Coq, ...) that can be exported afterwards to a system. However, only STTforall is supported currently
- *library* contains an arithmetic library (with a proof of Fermat's little theorem) written in Dedukti with the logic STTforall
- *src* contains the code that translate a proof from STTforall to one of the supported systems
- *src/export* contains for each supported system a syntactic translation from an AST of STTforall to the corresponding system
- *website* contains the HTML/CSS/PHP code for the website implemented using the Bootstrap framework
- *bdd* contains the mongodb code used by the website
- *bin* is outdated and contains a script to generate a thm file for OpenTheory
- *little_theorem* is outdated and contains a single file with the proof of Fermat's little theorem

# Adding a new target system

1) Add your system in `system.ml`
2) Implement an instance of the signature `E` in a module inside the `export` folder
3) Register your module in `export.ml`
4) Generate the database with `make library/<generated file> BDD=--export-web`

# Adding a new source system

Wait the Logipedia kick-off meeting
