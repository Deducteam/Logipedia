(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

open Extras
module B = Basic
module E = Entry
module T = Term
module U = Uri
module Jt = Json_types

exception IllTaxon
(** Exception raised when reading an ill formed taxon. *)

let deps = B.NameHashtbl.create 0

(** Specification of a taxon: conversion from Dedukti term. *)
module type TaxonSpec = sig
  type t

  val theory : string
  (** Name of the theory. *)

  val of_def : T.term -> t
  (** [of_def t] returns a taxon of a term [t] given that [t] comes
      from a definition. *)

  val of_decl : T.term -> t
  (** [of_def t] returns a taxon of a term [t] given that [t] comes
      from a declaration. *)

  val to_string : ?short:bool -> t -> string
  (** [to_string ?(short=false) tx] makes a long or [short] string out
      of taxon [tx]. *)

  val of_string : string -> t
  (** [of_string s] converts string [s] to a taxon.

      @raise IllTaxon if the string is not a taxon. *)

  val is_axiomatic : t -> bool
end

module Sttfa : TaxonSpec =
struct
  type t =
    | TxAxm (** Axiom *)
    | TxDef (** Definition *)
    | TxCst (** Constant *)
    | TxThm (** Theorem *)

  let theory = "sttfa"

  let of_def : T.term -> t = function
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    TxDef
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident "sttfa") ->
    TxThm
  | _ -> TxDef

  let of_decl : T.term -> t = function
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    TxCst
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident "sttfa") ->
    TxAxm
  | _ -> TxCst

  let to_string ?(short=false) tx =
    match tx with
    | TxAxm -> if short then "axm" else "axiom"
    | TxDef -> if short then "def" else "definition"
    | TxCst -> if short then "cst" else "constant"
    | TxThm -> if short then "thm" else "theorem"

  let of_string s =
    if s = "axiom" || s = "axm" then
      TxAxm
    else if s = "definition" || s = "def" then
      TxDef
    else if s = "constant" || s = "cst" then
      TxCst
    else if s = "theorem" || s = "thm" then
      TxThm
    else
      raise IllTaxon

  let is_axiomatic = (=) TxAxm
end

(** [find_taxon n] finds taxon of Dedukti name [n] across available
    json files. *)
let find_taxon : B.name -> Sttfa.t =
  (* Some memoization *)
  let taxons = B.NameHashtbl.create 0 in
  fun key ->
  try Basic.NameHashtbl.find taxons key
  with Not_found ->
    (* Parse the correct json file *)
    (* Output file must be in the same dir than other jsons *)
    let fname = B.md key |> B.string_of_mident in
    let fullpath = Filename.concat !(Jt.json_dir) (fname ^ ".json") in
    let doc = Yojson.Safe.from_file fullpath
              |> Jt.document_of_yojson
    in
    let f it =
      let uri = U.of_string it.Jt.name in
      let nm = U.name_of_uri uri in
      let tx = U.ext_of_uri uri |> Sttfa.of_string in
      B.NameHashtbl.add taxons nm tx
    in
    match doc with
    | Result.Error(s) ->
      failwith (Format.sprintf
                  "Error parsing file %s at line %s (as dependency)" fullpath s)
    | Result.Ok(doc) ->
      List.iter f doc;
      B.NameHashtbl.find taxons key

let find_theory : B.mident -> E.entry -> Dep.NameSet.t = fun mid e ->
  let id = E.id_of_entry e in
  Dep.compute_ideps := true;
  let name = B.mk_name mid id in
  Dep.ignore := true;
  begin try Dep.transitive_closure name
  with Dep.Dep_error(_) -> Format.printf "hatht" end;
  match Dep.get_data name with
  | exception Dep.Dep_error(_) -> Dep.NameSet.empty
  | d                                        ->
    let empty = { Dep.up = Dep.NameSet.empty
                ; Dep.down = Dep.NameSet.empty } in
    if d <> empty then Format.printf "hat";
    let ddep = d.Dep.down in
    let is_th n =
      ( (B.NameHashtbl.find deps n).Dep.up
        |> Dep.NameSet.filter (fun n -> Sttfa.is_axiomatic (find_taxon n)) )
      <> Dep.NameSet.empty
    in
    Dep.NameSet.filter is_th ddep
