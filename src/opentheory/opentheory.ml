open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `OpenTheory in
  M.print_ast fmt ast

module Makefile : Build_template.S =
struct
  include Sttfa.Makefile

  let rules_for md pth =
    let entries_pp fmt ens =
      Ast.compile md ens |> export fmt
    in
    Sttfa.Makefile.rules_for ~target:pth ~entries_pp md
end
