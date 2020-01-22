module A = Ast
module B = Kernel.Basic
module D = Core.Deps
module E = Parsing.Entry
module Denv = Api.Env.Default
module P = Parsing.Parser
open Core

let export_to_system_as_string : Systems.t -> Ast.item -> string = fun sys ->
  match sys with
  | Coq        -> Coq.string_of_item
  | Matita     -> Matita.string_of_item
  | OpenTheory -> Opentheory.string_of_item
  | Lean       -> Lean.string_of_item
  | Hollight   -> Hollight.string_of_item
  | Pvs        -> Pvs.string_of_item
  | _ -> assert false

let export_to_system_as_ast : Systems.t -> Format.formatter ->
  ?mdeps:Ast.mdeps -> Ast.ast -> unit = fun sys ->
  match sys with
  | Coq        -> Coq.print_ast
  | Matita     -> Matita.print_ast
  | OpenTheory -> Opentheory.print_ast
  | Lean       -> Lean.print_ast
  | Hollight   -> Hollight.print_ast
  | Pvs        -> Pvs.print_ast
  | _ -> assert false

(** [mk_ast md es] creates the STTfa ast of entries [es] from dedukti module
    [md] *)
let mk_ast : B.mident -> E.entry list -> A.ast = fun md entries ->
  let items = List.map (Compile.compile_entry md) entries in
  let fold_entry_dep dep e = D.QSet.union dep
      (D.dep_of_entry [Sttfadk.sttfa_module;md] e) in
  let dep = List.fold_left fold_entry_dep D.QSet.empty entries in
  { Ast.md = B.string_of_mident md; Ast.dep; items }

(*
let export_system : (module E) -> string -> Format.formatter -> unit =
  fun (module M:E) infile outfmt ->
  let md = Denv.init infile in
  let input = open_in infile in
  let entries = P.Parse_channel.parse md input in
  close_in input;
  let sttfa_ast = mk_ast md entries in
  M.print_ast outfmt sttfa_ast
*)
