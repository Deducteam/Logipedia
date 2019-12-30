open Core
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
  include Basis

  let mk_target f =
    let open Filename in
    (Option.get !outdir) </> !/f <.> file_ext

  let want = List.map (fun x -> Key.create @@ mk_target x)

  let rules_for files =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let outdir = Option.get !outdir in
    let finalize =
      Format.(
        Rule.phony ~deps:(want files)
          [sprintf "rename 's:-:_:g' %s/*.v" outdir;
           sprintf "cd %s && ls *.v > _CoqProject" outdir;
           sprintf "cd %s && coq_makefile -f _CoqProject -o Makefile" outdir;
           sprintf "cd %s && make" outdir]
          "finalize")
    in
    let files = List.map (fun x -> x, mk_target x) files in
    finalize :: rules_for files entries_pp

  let want files = Key.fake "finalize" :: want files
end
