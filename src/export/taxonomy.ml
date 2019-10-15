(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

module B = Kernel.Basic
module D = Api.Dep
module E = Parsers.Entry
module T = Kernel.Term
module U = Uri
module Jt = Json_types

exception IllTaxon
(** Exception raised when reading an ill formed taxon. *)

(** Specification of a taxon: conversion from Dedukti term. *)
module type TaxonSpec = sig
  type t

  val theory : string
  (** Name of the theory. *)

  val of_def : T.term option -> T.term -> t
  (** [of_def t u] returns a taxon of a term [t] with annotation [u] given that
      [t] comes from a definition. *)

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

  val label : t -> string * string option
  (** [label tx] returns labels for the fields {!Json_types.item.term} and
      {!Json_types.item.term_opt}. *)
end

module Sttfa : TaxonSpec =
struct
  type t =
    | TxAxm (** Axiom *)
    | TxDef (** Definition *)
    | TxCst (** Constant *)
    | TxThm (** Theorem *)

  let theory = "sttfa"

  let of_def : T.term option -> T.term -> t = fun t _ ->
    match t with
  | Some(App(Const(_,name),_,_)) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    TxDef
  | Some(App(Const(_,name),_,_)) when
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

  let label = function
    | TxCst -> ("type", None)
    | TxAxm -> ("statement", None)
    | TxDef -> ("body", Some("type_annotation"))
    | TxThm -> ("proof", Some("statement"))
end

