(** Compute taxonomy of Dedukti terms defined in sttfa theory *)

module F = Format
module Jt = Json_types
module B = Basic
module T = Term
module U = Uri
module S = Signature
module D = Dep
module Th = Theories

let find_tx_def : T.term -> U.taxon = function
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    U.TxDef
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident "sttfa") ->
    U.TxThm
  | _ -> U.TxDef

let find_tx_decl : T.term -> U.taxon = function
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident "sttfa") ->
    U.TxCst
  | App (Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident "sttfa") ->
    U.TxAxm
  | _ -> U.TxCst
