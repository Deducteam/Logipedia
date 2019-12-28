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
  module Bt = Build_template
  open Sttfa.Makefile
  include Basis

  let mk_target f =
    let open Filename in
    (Option.get !Bt.outdir) </> !/f <.> file_ext

  let rules_for files =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let files = List.map (fun x -> x, mk_target x) files in
    rules_for files entries_pp

  let want = List.map (fun x -> Bt.create @@ mk_target x)
end
