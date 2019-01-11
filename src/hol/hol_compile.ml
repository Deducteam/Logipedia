open Basic
open Entry
open Environ
open Hol
open Hol_ast


let rec compile_type ty =
  match ty with
    | Term.Kind -> assert false
    | Term.Type lc -> failwith ("Type \"Type\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
    | Term.DB (lc,_,_) -> failwith ("Type \"DB\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
    | Term.Const _ when is_hol_const hol_bool ty -> TBool
    | Term.Const (_, name) -> TDef (of_name name, [])
    | Term.App (Term.Const _ as a, ty1, [ty2]) when is_hol_const hol_arr a -> TArr (compile_type ty1, compile_type ty2)
    | Term.App (Term.Const (_, name), ty1, tyl) -> TDef (of_name name, (compile_type ty1)::(List.map compile_type tyl))
    | Term.App _ -> failwith "Type \"App\" not supported for HOL theory"
    | Term.Lam (lc,_,_,_) -> failwith ("Type \"Lam\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
    | Term.Pi (lc,_,_,_) -> failwith ("Type \"Pi\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))


let rec compile_term te =
  match te with
    | Term.Kind
    | Term.Type _ -> assert false
    | Term.DB (lc,_,_) -> failwith ("Term \"DB\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
    | Term.Const (_, name) -> Def (of_name name)
    | Term.App (Term.Const _ as e, ty, tl) when is_hol_const hol_eq e ->
       (let eq = Eq (compile_type ty) in
        match tl with
          | [] -> eq
          | _ -> App (eq, List.map compile_term tl))
    | Term.App (Term.Const (_, name), te1, tel) -> App (Def (of_name name), (compile_term te1)::(List.map compile_term tel))
    (* TODO: handle polymorphic constants applied to types *)
    | Term.App _ -> failwith "Type \"App\" not supported for HOL theory"
    | Term.Lam (lc,_,_,_) -> failwith ("Type \"Lam\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
    | Term.Pi (lc,_,_,_) -> failwith ("Type \"Pi\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))


let compile_definition lc name ty term =
  match ty with
    | _ when is_hol_const hol_type ty ->
       Format.eprintf "[COMPILE] type alias: %a@." Pp.print_name name ;
       TyAlias (of_name name, compile_type term)
    | Term.App (cst, a, []) when is_hol_const hol_term cst ->
       Format.eprintf "[COMPILE] term alias: %a@." Pp.print_name name ;
       TeAlias (of_name name, compile_type a, compile_term term)
    | _ -> failwith ("Entry \"Def\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))


let compile md e =
  match e with
  | Decl (lc, _, _, _) -> failwith ("Entry \"Decl\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Def (lc, id, opaque, Some ty, te) -> (
    Env.define lc id opaque te (Some ty);
    [ compile_definition lc (mk_name md id) ty te ]
  )
  | Def _ -> failwith "Definition without types are not supported for HOL theory"
  | Rules (lc,_) -> failwith ("Entry \"Rules\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Eval (lc,_,_) -> failwith ("Entry \"Eval\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Check (lc,_,_,_) -> failwith ("Entry \"Check\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Infer (lc,_,_) -> failwith ("Entry \"Infer\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Print (lc,_) -> failwith ("Entry \"Print\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | DTree (lc,_,_) -> failwith ("Entry \"DTree\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))
  | Name (lc,_) -> []
  | Require (lc,_) -> failwith ("Entry \"Require\" not supported for HOL theory at "^(Basic.string_of pp_loc lc))


let mk_ast md entries = List.flatten (List.map (compile md) entries)
