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
  module Bt = Build_template
  open Sttfa.Makefile
  include Basis

  let mk_target f =
    let open Filename in
    (Option.get !Bt.outdir) </> !/f <.> file_ext

  let rules_for files =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    let mkroot = (* Rule to create root file *)
      let open Filename in
      let outf = (Option.get !Bt.outdir) </> "root" in
      Bt.phony
        [Format.sprintf "echo 'baseuri = cic:/matita' > %s" outf]
        "matitaroot"
    in
    mkroot :: rules_for (List.map (fun x -> x, mk_target x) files) entries_pp

  let want files =
    `K_phon("matitaroot") ::
    List.map (fun x -> Bt.create @@ mk_target x) files
end
