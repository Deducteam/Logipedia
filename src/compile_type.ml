open Sttfadk
open Environ
open Ast

module E = Api.Env
module R = Kernel.Reduction
module P = Api.Pp.Default

let compile_tyop tyop =
  match tyop with T.Const (_, name) -> of_name name | _ -> assert false

let rec compile__type env _ty =
  match _ty with
  | T.Const (_, _) when is_sttfa_const sttfa_prop _ty -> Prop
  | T.App (c, left, [right]) when is_sttfa_const sttfa_arrow c ->
      let left' = compile__type env left in
      let right' = compile__type env right in
      Arrow (left', right')
  | T.DB (_, _, n) ->
      let var = get_dk_var env n in
      TyVar var
  | T.App (tyop, a, args) ->
      let tyop' = compile_tyop tyop in
      let args' = List.map (fun x -> compile__type env x) (a :: args) in
      TyOp (tyop', args')
  | T.Const _ -> TyOp (compile_tyop _ty, [])
  | _ -> assert false

let compile__type env _ty =
  let _ty = E.reduction ~ctx:env.dk
      ~red:{R.default_cfg with target=R.Snf} _ty  in
  compile__type env _ty

let rec compile_type (env: env) ty =
  match ty with
  | T.App (c, T.Lam (_, var, _, ty), [])
    when is_sttfa_const sttfa_forall_kind_type c ->
    let var = gen_fresh env var in
    let ty' = compile_type (add_ty_var_dk env var) ty in
    ForallK (soi var, ty')
  | T.App (c, a, []) when is_sttfa_const sttfa_p c ->
    Ty (compile__type env a)
  | _ -> assert false

let compile_wrapped__type env (ty: T.term) =
  match ty with
  | T.App (cst, T.App (c, a, []), [])
    when is_sttfa_const sttfa_etap cst && is_sttfa_const sttfa_p c ->
      compile__type env a
  | T.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      compile__type env a
  | _ ->
      Format.eprintf "%a@." P.print_term ty ;
      assert false


let compile_wrapped_type env (ty: T.term) =
  match ty with
  | T.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      compile_type env a
  | T.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      Ty (compile__type env a)
  | _ ->
      Format.eprintf "%a@." P.print_term ty ;
      assert false

let rec compile_type_definition env (ty: T.term) =
  match ty with
  | Term.Lam (_,x,_,ty) ->
    compile_type_definition (add_ty_var_dk env x) ty
  | _ ->
    let vars = env.ty in
    (vars, compile__type env ty)
