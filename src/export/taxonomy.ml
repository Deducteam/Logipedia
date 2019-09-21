(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

open Extras
module B = Basic
module E = Entry
module T = Term

(** Specification of a taxon: conversion from Dedukti term. *)
module type TaxonSpec = sig
  type t

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
