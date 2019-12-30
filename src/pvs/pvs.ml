open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Pvs in
  M.print_ast fmt ast

let file_ext = "pvs"

module Makefile : MAKEFILE =
struct
  open Build_template
  open Sttfa.Makefile
  include Basis

  let mk_target f =
    let open Filename in
    (Option.get !outdir) </> !/f <.> file_ext

  let rules_for files =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let chk f =
      let tg = mk_target f in
      Rule.check
      (Format.sprintf "proveit --importchain --scripts --force %s" tg) tg
    in
    rules_for (List.map (fun x -> x, mk_target x) files) entries_pp @
    List.map chk files

  let want = List.map (fun x -> Key.check @@ mk_target x)
end
