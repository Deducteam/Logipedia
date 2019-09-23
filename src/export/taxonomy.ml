(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

open Extras
module B = Basic
module E = Entry
module T = Term
module U = Uri
module Jt = Json_types

exception IllTaxon
(** Exception raised when reading an ill formed taxon. *)

(** Specification of a taxon: conversion from Dedukti term. *)
module type TaxonSpec = sig
  type t

  val taxons : t B.NameHashtbl.t
  (** Contains all taxons of file and dependencies.  Kind of
      memoization. *)

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
end

module Sttfa : TaxonSpec =
struct
  type t =
    | TxAxm (** Axiom *)
    | TxDef (** Definition *)
    | TxCst (** Constant *)
    | TxThm (** Theorem *)

  let taxons : t B.NameHashtbl.t = B.NameHashtbl.create 0

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
end

let taxonomise : B.mident -> E.entry list -> unit =
  fun md es ->
  let f e =
    (* TODO verify that dependencies have a taxon as well *)
    let idtx = match e with E.Decl(_,id,_,t) -> Some(id, Sttfa.of_decl t)
                          | E.Def(_,id,_,_,t) -> Some(id, Sttfa.of_def t)
                          | _ -> None
    in
    match idtx with
    | None -> ()
    | Some(id, tx) ->
      B.NameHashtbl.add Sttfa.taxons (B.mk_name md id) tx
  in
  List.iter f es

(** [tax_find_or_imp k txs] tries to find key [k] in taxons map [txs].
    If it fails, it tries to load the json where [k] is defined. *)
let tax_find_or_parse : B.NameHashtbl.key -> Sttfa.t = fun key ->
  try Basic.NameHashtbl.find Sttfa.taxons key
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
      B.NameHashtbl.add Sttfa.taxons nm tx
    in
    match doc with
    | Result.Error(s) ->
      failwith (Format.sprintf
                  "Error parsing file %s at line %s (as dependency)" fullpath s)
    | Result.Ok(doc) ->
      List.iter f doc;
      B.NameHashtbl.find Sttfa.taxons key
