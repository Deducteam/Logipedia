open Core
open Console
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Coq in
  M.print_ast fmt ast

let file_ext = "v"

module Makefile : MAKEFILE =
struct
  open Build_template
  open Sttfa.Makefile
  open Build.Classic
  include Basis

  let mk_target f =
    let open Filename in
    (Option.get !Cli.outdir) </> !/f <.> file_ext

  let want = List.map (fun x -> Key.create @@ mk_target x)

  (** [coqproject ()] is the _CoqProject file path. *)
  let coqproject : unit -> string = fun () ->
    Filename.(Option.get !Cli.outdir </> "_CoqProject")

  (** [coq_makefile ()] is the Coq Makefile path. *)
  let coq_makefile : unit -> string = fun () ->
    Filename.(Option.get !Cli.outdir </> "Makefile")

  (** [mk_coqproject l] creates a rule to write a coq project file for
      source files in [l] (so Dedukti files). *)
  let mk_coqproject : string list -> (key, value) rule = fun fs ->
    let vs = List.map mk_target fs in
    let outf = coqproject () in
    let write _ =
      log_rule ~lvl:3 "coqproject [%s]" outf;
      let ochan = open_out outf in
      let fmt = Format.formatter_of_out_channel ochan in
      List.iter (fun v -> Format.fprintf fmt "%s@\n" (Filename.basename v)) vs;
      close_out ochan;
      Value.Wfil (mtime outf)
    in
    let keys = List.map Key.create vs in
    target (Key.create outf) |> List.fold_right depends keys |> assemble write

  (** [mk_coq_makefile ()] creates a rule to write the makefile
      depending on the coqproject file. *)
  let mk_coq_makefile : unit -> (key, value) rule = fun () ->
    let outdir = Option.get !Cli.outdir in
    let cmd =
      Format.sprintf "cd %s && coq_makefile -f _CoqProject -o Makefile"
        outdir
    in
    Rule.sys cmd (coqproject ()) (coq_makefile ())

  (** [checkall ()] checks all coq files with [make]. *)
  let checkall : unit -> (key, value) rule = fun () ->
    let outdir = Option.get !Cli.outdir in
    Rule.phony ~deps:[Key.create (coq_makefile ())]
      [Format.sprintf "cd %s && make" outdir] "coqc"

  let rules_for files =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let fts = List.map (fun x -> x, mk_target x) files in
    mk_coqproject files :: mk_coq_makefile () :: checkall () ::
    rules_for fts entries_pp

  let want files = Key.fake "coqc" :: want files
end
