module A = Ast
module B = Kernel.Basic
module D = Core.Deps
module E = Parsing.Entry
module Denv = Api.Env.Default
module P = Parsing.Parser
open Core
open Extras

let export_to_system_as_string : Systems.t -> Ast.item -> string = fun sys ->
  match sys with
  | Coq        -> Coq.string_of_item
  | Matita     -> Matita.string_of_item
  | OpenTheory -> Opentheory.string_of_item
  | Lean       -> Lean.string_of_item
  | Hollight   -> Hollight.string_of_item
  | Pvs        -> Pvs.string_of_item
  | Agda       -> Agda.string_of_item
  | _ -> assert false

let export_to_system_as_ast : Systems.t -> Format.formatter ->
  ?mdeps:A.mdeps -> Ast.ast -> unit = fun sys ->
  match sys with
  | Coq        -> Coq.print_ast
  | Matita     -> Matita.print_ast
  | OpenTheory -> Opentheory.print_ast
  | Lean       -> Lean.print_ast
  | Hollight   -> Hollight.print_ast
  | Pvs        -> Pvs.print_ast
  | Agda       -> Agda.print_ast
  | _ -> assert false

(** [mk_ast md es] creates the STTfa ast of entries [es] from dedukti module
    [md] *)
let mk_ast : B.mident -> E.entry list -> A.ast = fun md entries ->
  let items = List.map (Compile.compile_entry md) entries in
  let fold_entry_dep dep e = D.QSet.union dep
      (D.dep_of_entry [Sttfadk.sttfa_module;md] e) in
  let dep = List.fold_left fold_entry_dep D.QSet.empty entries in
  { Ast.md = B.string_of_mident md; Ast.dep; items }

let get_sttfa_exporter : Systems.t -> (module Export.S) = fun target ->
  (module struct
    type ast = Ast.ast
    let target = target
    let compile = mk_ast
    let decompile _ = assert false
    let export : ast pp = fun fmt ast->
      export_to_system_as_ast target fmt ast
  end)
