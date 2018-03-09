open Basic
open Ast
open Sttforall

let soi = string_of_ident

let of_name name = string_of_mident (md name), string_of_ident (id name)

let cst_ty_arity = ref []

let rec take i l =
  if i = 0 then
    []
  else
    match l with
    | [] -> assert false
    | x::l -> x::(take (i-1) l)

let rec drop i l =
  if i = 0 then
    l
  else
    match l with
    | [] -> assert false
    | _::l -> drop (i-1) l

let compile_tyop tyop =
  match tyop with
  | Term.Const(_, name) ->
    of_name name
  | _ -> assert false

let rec compile__type ty_ctx _ty =
  match _ty with
  | Term.Const(_,cst) when is_sttfa_const sttfa_prop _ty -> Prop
  | Term.App(c,left,[right]) when is_sttfa_const sttfa_arrow c ->
    let left' = compile__type ty_ctx left in
    let right' = compile__type ty_ctx right in
    Arrow(left',right')
  | Term.DB(_,var,n) ->
    assert (List.mem (soi var) ty_ctx);
    TyVar(soi var)
  | Term.App(tyop,a,args) ->
    let tyop' = compile_tyop tyop in
    let args' = List.map (fun x -> compile__type ty_ctx x) (a::args) in
    TyOp(tyop',args')
  | Term.Const _ -> TyOp(compile_tyop _ty,[])
  | _ -> assert false


let rec compile_type ty_ctx ty =
  match ty with
  | Term.App(c, Term.Lam(_, var, _, ty), []) when is_sttfa_const sttfa_forall_kind_type c ->
    let ty' = compile_type ((soi var)::ty_ctx) ty in
    ForallK(soi var, ty')
  | Term.App(c, a, []) when is_sttfa_const sttfa_p c -> Ty(compile__type ty_ctx a)
  | _ -> assert false

let compile_wrapped_type (ty_ctx:ty_ctx) (ty:Term.term)  =
  match ty with
  | Term.App(cst, Term.App(c,a,[]), []) when (is_sttfa_const sttfa_etap cst && is_sttfa_const sttfa_p c)  ->
    compile__type ty_ctx a
  | _ ->  assert false

let rec compile__term te_ctx _te =
  match _te with
  | Term.DB(_,var,n) ->
    assert (List.mem_assoc (soi var) te_ctx.var);
    TeVar(soi var)
  | Term.Lam(_,id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
    let _te' = compile__term {te_ctx with ty = soi id::te_ctx.ty} _te in
    AbsTy(soi id, _te')
  | Term.Lam(_,id, Some tyvar, _te) ->
    let _ty' = compile_wrapped_type te_ctx.ty tyvar in
    let _te' = compile__term {te_ctx with var =(soi id,_ty')::te_ctx.var} _te in
    Abs(soi id, _ty', _te')
  | Term.App(cst, _ty, [Term.Lam(_,id, Some tyvar, _te)]) when is_sttfa_const sttfa_forall cst ->
    let _ty' = compile__type te_ctx.ty _ty in
    let _te' = compile__term {te_ctx with var = (soi id, _ty')::te_ctx.var} _te in
    Forall(soi id, _ty', _te')
  | Term.App(cst, tel, [ter]) when is_sttfa_const sttfa_impl cst ->
    let tel' = compile__term te_ctx tel in
    let ter' = compile__term te_ctx ter in
    Impl(tel',ter')
  | Term.App(Term.Const(_,name),a,args) ->
    let cst' = of_name name in
    assert (List.mem_assoc cst' !cst_ty_arity);
    let i = List.assoc cst' !cst_ty_arity in
    let args = a::args in
    let ty_args, te_args = take i args, drop i args in
    let ty_args' = List.map (compile__type te_ctx.ty)  ty_args in
    let te_args' = List.map (fun x -> compile__term
 te_ctx x) te_args in
    List.fold_left (fun app arg -> App(app,arg)) (Cst(cst', ty_args')) te_args'
  | Term.App(f,a,args) ->
    let f' = compile__term te_ctx f in
    let args' = List.map (fun x -> compile__term te_ctx x) (a::args) in
    List.fold_left (fun app arg -> App(app,arg)) f' args'
  | Term.Lam(_, _, None, _) -> failwith "lambda untyped are not supported"
  | Term.Const(lc,cst) ->  Cst(of_name cst, [])
  | _ -> assert false

let rec compile_term te_ctx te =
  match te with
  | Term.App(cst, Term.Lam(_,x, Some ty, te), []) when is_sttfa_const sttfa_forall_kind_prop cst ->
    assert (is_sttfa_const sttfa_type ty);
    let te' = compile_term {te_ctx with ty = (soi x)::te_ctx.ty} te in
    ForallP(soi x, te')
  | _ ->  Te(compile__term te_ctx te)

let compile_proof prf_ctx proof = failwith "todo"

let add_cst_ty_arity name i =
  cst_ty_arity := (name,i)::!cst_ty_arity

let rec arity_of_type ty =
  match ty with
  | ForallK(_, ty) -> 1+arity_of_type ty
  | _ -> 0

let compile_declaration name ty =
  Format.eprintf "%a@." pp_name name;
  match ty with
  | Term.App(cst,a,[]) when is_sttfa_const sttfa_etap cst ->
    let ty' = compile_type [] a in
    let i = arity_of_type ty' in
    add_cst_ty_arity (of_name name) i;
    Parameter(of_name name, compile_type [] a)
  | Term.App(cst,a,[]) when is_sttfa_const sttfa_eps cst ->
    Axiom(of_name name, compile_term {ty=[];var=[]} a)
  | Term.Const(_,_) when is_sttfa_const sttfa_type ty ->
    TyOpDef(of_name name, 0)
  | _ -> assert false

let compile_definition name ty term =
  match ty with
  | Term.App(cst,a,[]) when is_sttfa_const sttfa_etap cst ->
    let ty' = compile_type [] a in
    let i = arity_of_type ty' in
    add_cst_ty_arity (of_name name) i;
    Definition(of_name name, compile_type [] a, compile_term {ty=[];var=[]} term)
  | Term.App(cst,a,[]) when is_sttfa_const sttfa_eps cst ->
    Theorem(of_name name, compile_term {ty=[];var=[]} a, compile_proof [] term)
  | _ -> assert false
