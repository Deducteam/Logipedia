open Sttforall
open Environ
open Ast

let add_ty_var_dk env var =
  add_ty_var env (soi var)

let compile_tyop tyop =
  match tyop with Term.Const (_, name) -> of_name name | _ -> assert false

let rec compile__type env _ty =
  match _ty with
  | Term.Const (_, cst) when is_sttfa_const sttfa_prop _ty -> Prop
  | Term.App (c, left, [right]) when is_sttfa_const sttfa_arrow c ->
      let left' = compile__type env left in
      let right' = compile__type env right in
      Arrow (left', right')
  | Term.DB (_, var, n) ->
      let var = get_dk_var env n in
      TyVar var
  | Term.App (tyop, a, args) ->
      let tyop' = compile_tyop tyop in
      let args' = List.map (fun x -> compile__type env x) (a :: args) in
      TyOp (tyop', args')
  | Term.Const _ -> TyOp (compile_tyop _ty, [])
  | _ -> assert false

let rec compile_type (env: env) ty =
  match ty with
  | Term.App (c, Term.Lam (_, var, _, ty), [])
    when is_sttfa_const sttfa_forall_kind_type c ->
    let var = gen_fresh env var in
    let ty' = compile_type (add_ty_var_dk env var) ty in
    ForallK (soi var, ty')
  | Term.App (c, a, []) when is_sttfa_const sttfa_p c ->
      Ty (compile__type env a)
  | _ -> assert false

let compile_wrapped__type env (ty: Term.term) =
  match ty with
  | Term.App (cst, Term.App (c, a, []), [])
    when is_sttfa_const sttfa_etap cst && is_sttfa_const sttfa_p c ->
      compile__type env a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      compile__type env a
  | _ ->
      Format.eprintf "%a@." Pp.print_term ty ;
      assert false


let compile_wrapped_type env (ty: Term.term) =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      compile_type env a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      Ty (compile__type env a)
  | _ ->
      Format.eprintf "%a@." Pp.print_term ty ;
      assert false
