open Kernel.Basic
module Term = Kernel.Term

let sttfa_module = mk_mident "sttfa"
let sttfa_type = mk_ident "type"
let sttfa_ptype = mk_ident "ptype"
let sttfa_eta = mk_ident "eta"
let sttfa_etap = mk_ident "etap"
let sttfa_p = mk_ident "p"
let sttfa_arrow = mk_ident "arrow"
let sttfa_forall = mk_ident "forall"
let sttfa_leibniz = mk_ident "leibniz"
let sttfa_impl = mk_ident "impl"
let sttfa_prop = mk_ident "bool"
let sttfa_eps = mk_ident "eps"
let sttfa_forall_kind_type = mk_ident "forallK"
let sttfa_forall_kind_prop = mk_ident "forallP"

let is_sttfa_const c t =
  match t with
  | Term.Const(_, cst) -> name_eq cst (mk_name sttfa_module c)
  | _ -> false

let rec is_tyop ty =
  match ty with
  | Term.Const _ when is_sttfa_const sttfa_type ty -> true
  | Term.Pi(_,_,l,r) when is_sttfa_const sttfa_type l -> is_tyop r
  | _ -> false

let rec arity_of_tyop ty =
  match ty with
  | Term.Const _ -> 0
  | Term.Pi(_,_,_,r) -> 1 + arity_of_tyop r
  | _ -> assert false
