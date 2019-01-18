open Basic
open Entry
open Environ

open Hol
open Ast


(** Going from dedukti+hol to STTFA ast **)

(* let rec compile_type ty =
 *   match ty with
 *     | Term.Kind -> assert false
 *     | Term.Type lc -> failwith ("Type \"Type\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
 *     | Term.DB (lc,_,_) -> failwith ("Type \"DB\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
 *     | Term.Const _ when is_hol_const hol_bool ty -> TBool
 *     | Term.Const (_, name) -> TDef (of_name name, [])
 *     | Term.App (Term.Const _ as a, ty1, [ty2]) when is_hol_const hol_arr a -> TArr (compile_type ty1, compile_type ty2)
 *     | Term.App (Term.Const (_, name), ty1, tyl) -> TDef (of_name name, (compile_type ty1)::(List.map compile_type tyl))
 *     | Term.App _ -> failwith "Type \"App\" not supported for HOL theory"
 *     | Term.Lam (lc,_,_,_) -> failwith ("Type \"Lam\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
 *     | Term.Pi (lc,_,_,_) -> failwith ("Type \"Pi\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
 * 
 * 
 * let rec compile_term te =
 *   match te with
 *     | Term.Kind
 *     | Term.Type _ -> assert false
 *     | Term.DB (lc,_,_) -> failwith ("Term \"DB\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
 *     | Term.Const (_, name) -> Def (of_name name)
 *     | Term.App (Term.Const _ as e, ty, tl) when is_hol_const hol_eq e ->
 *        (let eq = Eq (compile_type ty) in
 *         match tl with
 *           | [] -> eq
 *           | _ -> App (eq, List.map compile_term tl))
 *     | Term.App (Term.Const (_, name), te1, tel) -> App (Def (of_name name), (compile_term te1)::(List.map compile_term tel))
 *     (\* TODO: handle polymorphic constants applied to types *\)
 *     | Term.App _ -> failwith "Type \"App\" not supported for HOL theory"
 *     | Term.Lam (lc,_,_,_) -> failwith ("Type \"Lam\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
 *     | Term.Pi (lc,_,_,_) -> failwith ("Type \"Pi\" not supported for HOL theory at "^(Basic.string_of pp_loc lc)) *)


let rec subst_type ty v t =
  match ty with
    | TyVar w -> if v = w then t else ty
    | Arrow (a, b) -> Arrow (subst_type a v t, subst_type b v t)
    | TyOp (n, l) -> TyOp (n, List.map (fun ty -> subst_type ty v t) l)
    | Prop -> Prop


let rec subst_ptype ty v t =
  match ty with
    | ForallK (w, ty) -> ForallK (w, subst_ptype ty v t)
    | Ty ty -> Ty (subst_type ty v t)


let rec apply_type ty tyl =
  match ty, tyl with
    | Ty ty, [] -> ty
    | ForallK (v, ty), t::tyl ->
       let ty = subst_ptype ty v t in
       apply_type ty tyl
    | _, _ -> assert false


let rec compile_type lc env vars ty =
  match ty with
    | Term.DB (_, _, n) -> TyVar (List.nth (List.rev vars) n)
    | Term.App (a, ty1, [ty2]) when is_hol_const hol_arr a ->
       let ty1 = compile_type lc env vars ty1 in
       let ty2 = compile_type lc env vars ty2 in
       Arrow (ty1, ty2)
    | Term.App ((Term.Const (_, f)), ty, tyl) ->
       let f = List.assoc f env in
       apply_type f (List.map (compile_type lc env vars) (ty::tyl))
    | _ -> failwith ("HOL type not supported yet at "^(Basic.string_of pp_loc lc))


let rec compile_dk_type ty =
  match ty with
    | Term.Pi (_, _, dom, cod) when is_hol_const hol_type dom ->
       let (nvars, cod) = compile_dk_type cod in
       (nvars+1, cod)
    | Term.Pi (lc, _, _, _) -> failwith ("Error at "^(Basic.string_of pp_loc lc))
    | _ -> (0, ty)


let rec compile_dk_term nvars te =
  if nvars = 0 then
    ([], te)
  else
    match te with
      | Term.Lam (_, x, _, te) ->
         let (vars, res) = compile_dk_term (nvars-1) te in
         ((Basic.string_of_ident x)::vars, res)
      | _ -> assert false


let compile_definition lc env name ty term =
  let (nvars, cod) = compile_dk_type ty in
  let (vars, res) = compile_dk_term nvars term in
  if is_hol_const hol_type cod then
    let ty = compile_type lc env vars res in
    let ty = List.fold_right (fun x t -> ForallK (x, t)) vars (Ty ty) in
    (None, (name, ty)::env)
  else
    failwith ("Term definitions not supported yet for HOL theory at "^(Basic.string_of pp_loc lc))


(* env contain type definitions: currently they are unfolded since they
   cannot be represented in the STTFA ast *)
let compile env md e =
  match e with
  | Decl (lc, _, _, _) -> failwith ("Entry \"Decl\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Def (lc, id, opaque, Some ty, te) -> (
    Env.define lc id opaque te (Some ty);
    compile_definition lc env (mk_name md id) ty te
  )
  | Def _ -> failwith "Definition without types are not supported for HOL theory"
  | Rules (lc,_) -> failwith ("Entry \"Rules\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Eval (lc,_,_) -> failwith ("Entry \"Eval\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Check (lc,_,_,_) -> failwith ("Entry \"Check\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Infer (lc,_,_) -> failwith ("Entry \"Infer\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Print (lc,_) -> failwith ("Entry \"Print\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | DTree (lc,_,_) -> failwith ("Entry \"DTree\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Name (lc,_) -> (None, env)
  | Require (lc,_) -> failwith ("Entry \"Require\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))


let mk_ast md entries =
  List.fold_left (fun (res, env) e ->
      match compile env md e with
        | (Some r, env) -> (r::res, env)
        | (None, env) -> (res, env)
    ) ([], []) entries
