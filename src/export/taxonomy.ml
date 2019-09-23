(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

open Extras
module B = Basic
module E = Entry
module T = Term
module U = Uri
module Jt = Json_types

(** Specification of a taxon: conversion from Dedukti term. *)
module type TaxonSpec = sig
  type t

  val theory : string
  (** Name of the theory. *)

  val default : t
  (** A default label if no other is available. *)
  (* Should be removed: all items ought to have a taxon. *)

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
  (** [of_string s] converts string [s] to a taxon. *)
end

module Sttfa : TaxonSpec =
struct
  type t =
    | TxAxm (** Axiom *)
    | TxDef (** Definition *)
    | TxCst (** Constant *)
    | TxThm (** Theorem *)
    | TxNa  (** Not available *)

  let default = TxNa

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
    | TxNa  -> if short then "na"  else "N/A"

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
      TxNa
end

let taxonomise : B.mident -> E.entry list -> Sttfa.t Str2Map.t =
  fun md es ->
  let f acc e =
    (* TODO verify that dependencies have a taxon as well *)
    let idtx = match e with E.Decl(_,id,_,t) -> Some(id, Sttfa.of_decl t)
                          | E.Def(_,id,_,_,t) -> Some(id, Sttfa.of_def t)
                          | _ -> None
    in
    match idtx with
    | None -> acc
    | Some(id, tx) ->
      let key = (B.string_of_mident md, B.string_of_ident id) in
      Str2Map.add key tx acc
  in
  List.fold_left f Str2Map.empty es

(** [tax_find_or_imp k txs] tries to find key [k] in taxons map [txs].
    If it fails, it tries to load the json where [k] is defined.
    [k] is [(md, id)] where [md] and [id] are strings coming from
    [mident] and [ident].
    TODO use mident and ident rather than string string *)
let tax_find_or_parse : Str2Map.key -> Sttfa.t Str2Map.t
  -> Sttfa.t =
  fun key s2map ->
  try Str2Map.find key s2map
  with Not_found ->
    (* Parse the correct json file *)
    (* Output file must be in the same dir than other jsons *)
    let fullpath = Filename.concat !(Jt.json_dir) ((fst key) ^ ".json") in
    let doc = Yojson.Safe.from_file fullpath
              |> Jt.document_of_yojson
    in
    let f acc it =
      let uri = U.of_string it.Jt.name in
      let nm = U.name_of_uri uri in
      let key = (B.string_of_mident @@ B.md nm, B.string_of_ident @@ B.id nm) in
      let tx = U.ext_of_uri uri |> Sttfa.of_string in
      Str2Map.add key tx acc
    in
    match doc with
    | Result.Error(s) ->
      failwith (Format.sprintf
                  "Error parsing file %s at line %s (as dependency)" fullpath s)
    | Result.Ok(doc) ->
      let s2map = List.fold_left f s2map doc in
      Str2Map.find key s2map
