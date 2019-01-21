open Basic
open Entry
open Environ

open Hol
open Ast


(** Going from dedukti+hol to STTFA ast **)

(* Everything relies on the fact that type variables are abstracted
   first, then term variables *)

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


let rec compile_type lc env tvars nvars ty =
  match ty with
    | Term.DB (lc, _, n) ->
       (try
          TyVar (List.nth (List.rev tvars) (n - nvars))
        with
          | Failure s -> failwith ("s at "^(Basic.string_of pp_loc lc))
          | Invalid_argument s -> raise (Invalid_argument ("s at "^(Basic.string_of pp_loc lc))))
    | Term.App (a, ty1, [ty2]) when is_hol_const hol_arr a ->
       let ty1 = compile_type lc env tvars nvars ty1 in
       let ty2 = compile_type lc env tvars nvars ty2 in
       Arrow (ty1, ty2)
    | Term.App ((Term.Const (_, f)), ty, tyl) ->
       let f = List.assoc f env in
       apply_type f (List.map (compile_type lc env tvars nvars) (ty::tyl))
    | _ -> failwith ("HOL type not supported yet at "^(Basic.string_of pp_loc lc))


let rec compile_term lc env tvars vars te =
  match te with
    | Term.DB (lc, _, n) ->
       (try
          TeVar (fst (List.nth (List.rev vars) n))
        with Failure s -> failwith ("s at "^(Basic.string_of pp_loc lc)))
    | _ -> failwith ("HOL term not supported yet at "^(Basic.string_of pp_loc lc))



let rec compile_dk_type env tvars vars ty =
  match ty with
    | Term.Pi (_, x, dom, cod) when is_hol_const hol_type dom ->
       compile_dk_type env ((Basic.string_of_ident x)::tvars) vars cod
    | Term.Pi (lc, _, Term.App(t, ty, _), cod) when is_hol_const hol_term t ->
       let ty = compile_type lc env tvars (List.length vars) ty in
       compile_dk_type env tvars (ty::vars) cod
    | _ -> (tvars, vars, ty)


let rec compile_dk_term env tvars vars te =
  match te with
    | Term.Lam (_, x, (Some dom), te) when is_hol_const hol_type dom ->
       compile_dk_term env ((Basic.string_of_ident x)::tvars) vars te
    | Term.Lam (lc, x, (Some (Term.App (t, ty, _))), te) when is_hol_const hol_term t ->
       let ty = compile_type lc env tvars (List.length vars) ty in
       compile_dk_term env tvars ((Basic.string_of_ident x, ty)::vars) te
    | _ -> (tvars, vars, te)


let compile_definition lc env name ty term =
  let (tvarsty, varsty, cod) = compile_dk_type env [] [] ty in
  let (tvarste, varste, res) = compile_dk_term env [] [] term in
  match cod with
    | _ when is_hol_const hol_type cod ->
       assert (varste = []);
       let ty = compile_type lc env tvarste 0 res in
       let ty = List.fold_right (fun x t -> ForallK (x, t)) tvarste (Ty ty) in
       (None, (name, ty)::env)
    | Term.App (t, ty, _) when is_hol_const hol_term t ->
       let ty = compile_type lc env tvarsty (List.length varsty) ty in
       let ty = List.fold_right (fun t ty -> Arrow (t, ty)) varsty ty in
       let ty = List.fold_right (fun x ty -> ForallK (x, ty)) tvarsty (Ty ty) in
       let te = compile_term lc env tvarste varste res in
       let te = List.fold_right (fun (x, t) ty -> Abs (x, t, ty)) varste te in
       let te = List.fold_right (fun x ty -> ForallP (x, ty)) tvarste (Te te) in
       (Some (Definition (of_name name, ty, te)), env)
    (* | Term.App(t, _, _) when is_hol_const hol_proot t ->
     *    let jp = compile_proof lc env tvars vars res in
     *    let jp =
     *      List.fold_right (fun (x, ty) (j, p) ->
     *          let ty = compile_type lc env tvars ty in
     *          let (Te thm) = j.thm in
     *          let j = {ty = j.ty ; te = tl j.te ; hyp = j.hyp ; thm = Forall (x, ty, thm)} in
     *          (j, ForallI (j, p, x))) vars jp
     *    in
     *    let (_, p) =
     *      List.fold_right (fun x (j, p) ->
     *          let j = {ty = tl j.ty ; te = j.te ; hyp = j.hyp ; thm = ForallP (x, j.thm)} in
     *          (j, ForallPI (j, p, x))) vars jp
     *    in
     *    (Some (Theorem (name, ??, p))) *)
    | _ -> failwith ("Unsupported definition for HOL theory at "^(Basic.string_of pp_loc lc))


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
