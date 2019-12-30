open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Matita in
  M.print_ast fmt ast

let file_ext = "matita"

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
    let mkroot = (* Rule to create root file *)
      let open Filename in
      let outf = (Option.get !outdir) </> "root" in
      Rule.phony
        [Format.sprintf "echo 'baseuri = cic:/matita' > %s" outf]
        "matitaroot"
    in
    mkroot :: rules_for (List.map (fun x -> x, mk_target x) files) entries_pp

  let want files =
    Key.fake "matitaroot" ::
    List.map (fun x -> Key.create @@ mk_target x) files
end
