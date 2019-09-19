(** Computing dependencies of a given Json ppterm *)

open Extras
module Jt = Json_types
module U = Uri

module NameSet = Set.Make(String)

let option_union : NameSet.t -> (NameSet.t option) -> NameSet.t = fun s so ->
  match so with
  | Some s' -> NameSet.union s s'
  | None -> s

let rec direct_deps : Jt.Ppterm.t -> NameSet.t = function
  | Var(v) -> List.fold_right (NameSet.union) (List.map direct_deps v.v_args) NameSet.empty
  | Binder(b) ->
    let deps_args = List.fold_right (NameSet.union) (List.map direct_deps b.b_args) NameSet.empty in
    let deps_body = direct_deps b.body in
    let deps_annotation = Option.map direct_deps b.annotation in
    option_union (NameSet.union deps_args deps_body) deps_annotation
  | Const(c) ->
    let deps_args = List.fold_right (NameSet.union) (List.map direct_deps c.c_args) NameSet.empty in
    NameSet.add c.c_symb deps_args
