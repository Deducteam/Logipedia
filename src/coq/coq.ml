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
  include Basis

  let mk_target f =
    let open Filename in
    (Option.get !Cli.outdir) </> !/f <.> file_ext

  let want = List.map (fun x -> Key.create @@ mk_target x)

  let rules_for files =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let fts = List.map (fun x -> x, mk_target x) files in
    rules_for fts entries_pp

  let want files = want files
end
