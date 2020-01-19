open Core
open Console
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Lean in
  M.print_ast fmt ast

let file_ext = "lean"

module Makefile : MAKEFILE =
struct
  open Build_template
  open Sttfa.Makefile
  open Filename
  include Basis

  let mk_target f = (Option.get !Cli.outdir) </> !/f <.> file_ext

  let generators =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    mk_generators ~.file_ext entries_pp

  let want = List.map (fun x -> Key.create @@ mk_target x)
end
