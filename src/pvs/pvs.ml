open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Pvs in
  M.print_ast fmt ast

module Makefile : MAKEFILE =
struct
  open Sttfa.Makefile
  include Basis

  let rules_for files mk_target =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let chk f =
      let tg = mk_target f in
      Build_template.check_with
      (Format.sprintf "proveit --importchain --scripts --force %s" tg) tg
    in
    Sttfa.Makefile.rules_for files mk_target entries_pp @
    List.map chk files

  let want : path -> key = check
end
