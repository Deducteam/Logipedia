(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

open Extras
module B = Basic
module D = Dep
module E = Entry
module T = Term
module U = Uri
module Jt = Json_types

exception IllTaxon
(** Exception raised when reading an ill formed taxon. *)

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
  (** [is_axiomatic t] is true if taxon [t] should be considered as an
      axiom. *)
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

  let is_axiomatic : t -> bool = (=) TxAxm
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

(** [find_deps m i e] computes the list of all direct down
    dependencies of a Dedukti entry [e] with name [m.i] as a list of
    Dedukti names. *)
let find_deps : B.mident -> E.entry -> B.name list = fun mid e ->
  let id = match e with
    | E.Decl(_,id,_,_)
    | E.Def(_,id,_,_,_) -> id
    | _                 -> assert false
  in
  D.compute_ideps := true; (* Compute dependencies of items *)
  D.make mid [e];
  let name = B.mk_name mid id in
  match D.get_data name with
  | exception D.Dep_error(D.NameNotFound(_)) -> []
  | d ->
    (* Remove some elements from dependencies and create a part of the uri. *)
    let f n = B.string_of_mident (B.md n) = Sttfa.theory in
    List.filter f (D.NameSet.elements D.(d.down))

type memo_entry = New of E.entry | Memo of B.ident

(** [find_deps eoi mid] computes the list of all direct down dependencies of
    Dedukti entry [entry] coming from module [mid]. If [eoi] is
    {!constructor:New}, then the dedukti file is used to build the dependencies.
    If [e] is {!constructor:Memo(i)}, then the function relies on the fact that
    the dependencies for [mid.i] have already been computed.

    @raise Not_found if [eoi] is {!constructor:Memo(i)} and the dependencies of
    [i] haven't been computed yet. *)
let find_deps : B.mident -> memo_entry -> B.name list = fun mid entry ->
  match entry with
  | Memo(_) -> failwith "Memoization not implemented yet"
  | New(e)  -> find_deps mid e

let rec thax : B.name -> Dep.NameSet.t =
  let thaxs = B.NameHashtbl.create 0 in
  fun key ->
  try B.NameHashtbl.find thaxs key
  with Not_found ->
    let fname = B.md key |> B.string_of_mident in
    let fullpath = Filename.concat !(Jt.json_dir) (fname ^ ".json") in
    let doc = Yojson.Safe.from_file fullpath |> Jt.document_of_yojson in
    let f it : unit =
      let uri = U.of_string it.Jt.name in
      let nm = U.name_of_uri uri in
      let addition =
        match Sttfa.is_axiomatic @@ Sttfa.of_string it.Jt.taxonomy,
              it.Jt.deps with
        | _    , []   -> Dep.NameSet.singleton nm
        | true, deps ->
          let names = List.map
              (fun x -> U.name_of_uri @@ U.of_string x) deps
          in
          let thxd = List.fold_right
              (fun e acc -> Dep.NameSet.union (thax e) acc)
              names Dep.NameSet.empty
          in
          Dep.NameSet.add nm thxd
        | _, _        -> Dep.NameSet.empty
      in
      B.NameHashtbl.add thaxs nm addition
    in
    match doc with
    | Result.Error(s) ->
      failwith @@
      Format.sprintf "Error parsing file %s at line %s (as dependency)"
        fullpath s
    | Result.Ok(doc)  ->
      List.iter f doc;
      B.NameHashtbl.find thaxs key
