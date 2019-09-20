(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

module F = Format
module Jt = Json_types
module B = Basic
module T = Term
module U = Uri
module S = Signature
module D = Dep
module Th = Theories

(** Specification of a taxon: conversion from Dedukti term. *)
module type TaxonSpec = sig
  val of_def : T.term -> U.taxon
  val of_decl : T.term -> U.taxon
end

module Sttfa : TaxonSpec =
struct
  let of_def : T.term -> U.taxon = function
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    U.TxDef
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident "sttfa") ->
    U.TxThm
  | _ -> U.TxDef

  let of_decl : T.term -> U.taxon = function
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    U.TxCst
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident "sttfa") ->
    U.TxAxm
  | _ -> U.TxCst
end
