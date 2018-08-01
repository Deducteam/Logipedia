open Sttforall
open Ast
open Basic
open Term

let to_const ident = Term.mk_Const dloc (mk_name sttfa_module ident)

let to_name (md, id) = mk_name (mk_mident md) (mk_ident id)

let rec find name (ctx:typed_context) i =
  match ctx with
  | [] ->   Format.eprintf "debug: %s@." name; assert false
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
  | Cst (cst, args) ->
    let args' = List.map (decompile__type ctx) args in
    Term.mk_App2 (Term.mk_Const dloc (to_name cst)) args'


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

let to_eps t = Term.mk_App (to_const sttfa_eps) t []

let add_prf_ctx ctx id _te  = (Basic.dloc, mk_ident id, _te) :: ctx

let rec decompile_proof ctx prf =
  match prf with
  | Assume(_,var) -> Term.mk_DB dloc (mk_ident var) (find var ctx)
  | Lemma(cst,_) -> Term.mk_Const dloc (to_name cst)
  | Conv(_,prf,tr) -> decompile_proof ctx prf
  | ImplE(j,left,right) -> Term.mk_App (decompile_proof ctx left) (decompile_proof ctx right) []
  | ImplI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_te = TeSet.choose (TeSet.filter (fun (x,te) -> if x = var then true else false) j'.hyp) in
    let _te' = decompile__term ctx _te in
    let ctx' = add_prf_ctx ctx var _te' in
    let proof' = decompile_proof ctx' proof in
    Term.mk_Lam dloc (mk_ident var) (Some (to_eps _te')) proof'
  | ForallE(j,left,_te) ->
    let _te' = decompile__term ctx _te in
    Term.mk_App (decompile_proof ctx left) _te' []
  | ForallI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_) -> if x = var then true else false) j'.te in
    let _ty' = decompile__type ctx _ty in
    let ctx' = add_te_var var _ty' ctx in
    let proof' = decompile_proof ctx' proof in
    Term.mk_Lam dloc (mk_ident var) (Some (to__type _ty')) proof'
  | ForallPE(j,left,_ty) ->
    let _ty' = decompile__type ctx _ty in
    Term.mk_App (decompile_proof ctx left) _ty' []
  | ForallPI(j,proof,var) ->
    let ctx' = add_ty_var var ctx in
    let proof' = decompile_proof ctx' proof in
    Term.mk_Lam dloc (mk_ident var) (Some (to_const sttfa_type)) proof'
