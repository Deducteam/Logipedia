open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Lean in
  M.print_ast fmt ast

module Makefile : MAKEFILE =
struct
  open Build_template

  type key =
    [ `Kfile of path
    | `Ksign of mident ]

  type value =
    [ `Vfile of path * float
    | `Vsign of entry list ]

  let key_eq = Dk.key_eq
  let pp_key = Dk.pp_key
  let valid_stored = Dk.valid_stored
  let want : path -> key = Dk.want

  let rules_for files mk_target =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    Sttfa.Makefile.rules_for files mk_target entries_pp
end
