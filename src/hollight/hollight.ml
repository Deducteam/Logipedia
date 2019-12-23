open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Hollight in
  M.print_ast fmt ast

module Makefile : Build_template.S =
struct
  type key = Sttfa.Makefile.key
  type value = Sttfa.Makefile.value

  let pp_key = Sttfa.Makefile.pp_key
  let key_eq = Sttfa.Makefile.key_eq
  let valid_stored = Sttfa.Makefile.valid_stored
  let want = Sttfa.Makefile.want
  let rules_for md pth =
    let entries_pp fmt ens =
      Ast.compile md ens |> export fmt
    in
    Sttfa.Makefile.rules_for ~target:pth ~entries_pp md
end
