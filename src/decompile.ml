open Sttforall
open Ast
open Basic
open Term

let to_const ident = Term.mk_Const dloc (mk_name sttfa_module ident)

let to_name (md, id) = mk_name (mk_mident md) (mk_ident id)

let rec find name (ctx:typed_context) i =
  match ctx with
  | [] -> assert false
  | (_,x,_) :: t -> if mk_ident name = x then i else find name t (i + 1)


let find name ctx = find name ctx 0

let add_ty_var ty_var ctx =
  (dloc,mk_ident ty_var, to_const sttfa_type)::ctx

let rec decompile__type ctx _ty =
  match _ty with
  | Prop -> to_const sttfa_prop
  | Arrow (_tyl, _tyr) ->
      Term.mk_App (to_const sttfa_arrow)
        (decompile__type ctx _tyl)
        [decompile__type ctx _tyr]
  | TyVar string -> Term.mk_DB dloc (mk_ident string) (find string ctx)
  | TyOp (tyop, []) -> Term.mk_Const dloc (to_name tyop)
  | TyOp (tyop, _ty :: _args) ->
      Term.mk_App
        (Term.mk_Const dloc (to_name tyop))
        (decompile__type ctx _ty)
        (List.map (decompile__type ctx) _args)

let rec decompile_type ctx ty =
  match ty with
  | Ty ty -> decompile__type ctx ty
  | ForallK(ty_var, ty) ->
      let ty' = decompile_type (add_ty_var ty_var ctx) ty in
      Term.mk_App
        (to_const sttfa_forall_kind_type)
        (Term.mk_Lam dloc (mk_ident ty_var) (Some (to_const sttfa_type)) ty')
        []


let to__type _ty =
  Term.mk_App (to_const sttfa_etap) (Term.mk_App (to_const sttfa_p) _ty []) []

let add_te_var te_var _ty ctx =
  (dloc,mk_ident te_var, to__type _ty)::ctx

(* ASSUMPTION: the set of ty_var and te_var are disjoint *)
let rec decompile__term ctx _te =
  match _te with
  | TeVar x -> Term.mk_DB dloc (mk_ident x) (find x ctx)
  | Abs (x, _ty, _te) ->
    let _ty' = decompile__type ctx _ty in
    let ctx' = add_te_var x _ty' ctx in
    Term.mk_Lam dloc (mk_ident x) (Some (to__type _ty'))
        (decompile__term ctx' _te)
  | App (tel, ter) ->
      Term.mk_App (decompile__term ctx tel) (decompile__term ctx ter) []
  | Forall (te_var, _ty, _te) ->
    let _ty' = decompile__type ctx _ty in
    let ctx' = add_te_var te_var _ty' ctx in
    let _te' = decompile__term ctx' _te in
    Term.mk_App (to_const sttfa_forall) _ty'
      [Term.mk_Lam dloc (mk_ident te_var) (Some (to__type _ty')) _te']
  | Impl (_tel, _ter) ->
      let _tel' = decompile__term ctx _tel in
      let _ter' = decompile__term ctx _ter in
      Term.mk_App (to_const sttfa_impl) _tel' [_ter']
  | AbsTy (ty_var, _te) ->
    let ctx' = add_ty_var ty_var ctx in
      Term.mk_Lam dloc (mk_ident ty_var) (Some (to_const sttfa_type))
        (decompile__term ctx' _te)
  | Cst (cst, []) -> Term.mk_Const dloc (to_name cst)
  | Cst (cst, x :: args) ->
      let x' = decompile__type ctx x in
      let args' = List.map (decompile__type ctx) args in
      Term.mk_App (Term.mk_Const dloc (to_name cst)) x' args'


let rec decompile_term ctx te =
  match te with
  | Te _te -> decompile__term ctx _te
  | ForallP (ty_var, te) ->
    let ctx' = add_ty_var ty_var ctx in
    let te' = decompile_term ctx' te in
      Term.mk_App
        (to_const sttfa_forall_kind_prop)
        (Term.mk_Lam dloc (mk_ident ty_var) (Some (to_const sttfa_type)) te')
        []
