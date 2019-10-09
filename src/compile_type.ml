module Envi = Environ

module A = Ast
module E = Api.Env
module R = Kernel.Reduction
module P = Api.Pp.Default
module T = Kernel.Term
module S = Sttfadk

let compile_tyop tyop =
  match tyop with T.Const (_, name) -> Envi.of_name name | _ -> assert false

let rec compile__type env _ty =
  match _ty with
  | T.Const (_, _) when S.is_sttfa_const S.sttfa_prop _ty -> A.Prop
  | T.App (c, left, [right]) when S.is_sttfa_const S.sttfa_arrow c ->
      let left' = compile__type env left in
      let right' = compile__type env right in
      Arrow (left', right')
  | T.DB (_, _, n) ->
      let var = Envi.get_dk_var env n in
      TyVar var
  | T.App (tyop, a, args) ->
      let tyop' = compile_tyop tyop in
      let args' = List.map (fun x -> compile__type env x) (a :: args) in
      TyOp (tyop', args')
  | T.Const _ -> TyOp (compile_tyop _ty, [])
  | _ -> assert false

let compile__type (dkenv:E.t) env (_ty:T.term) =
  let _ty = E.reduction dkenv ~ctx:env.Envi.dk
      ~red:{R.default_cfg with target=R.Snf} _ty  in
  compile__type env _ty

let rec compile_type (dkenv:E.t) (env: Envi.env) ty =
  match ty with
  | T.App (c, T.Lam (_, var, _, ty), [])
    when S.is_sttfa_const S.sttfa_forall_kind_type c ->
    let var = Envi.gen_fresh env var in
    let ty' = compile_type dkenv (Envi.add_ty_var_dk env var) ty in
    A.ForallK (Envi.soi var, ty')
  | T.App (c, a, []) when S.is_sttfa_const S.sttfa_p c ->
    Ty (compile__type dkenv env a)
  | _ -> assert false

let compile_wrapped__type dkenv env (ty: T.term) =
  match ty with
  | T.App (cst, T.App (c, a, []), [])
    when S.is_sttfa_const S.sttfa_etap cst && S.is_sttfa_const S.sttfa_p c ->
      compile__type dkenv env a
  | T.App (cst, a, []) when S.is_sttfa_const S.sttfa_eta cst ->
      compile__type dkenv env a
  | _ ->
      Format.eprintf "%a@." P.print_term ty ;
      assert false


let compile_wrapped_type dkenv env (ty: T.term) =
  match ty with
  | T.App (cst, a, []) when S.is_sttfa_const S.sttfa_etap cst ->
      compile_type dkenv env a
  | T.App (cst, a, []) when S.is_sttfa_const S.sttfa_eta cst ->
      A.Ty (compile__type dkenv env a)
  | _ ->
      Format.eprintf "%a@." P.print_term ty ;
      assert false

let rec compile_type_definition (dkenv:E.t) env (ty: T.term) =
  match ty with
  | T.Lam (_,x,_,ty) ->
    compile_type_definition dkenv (Envi.add_ty_var_dk env x) ty
  | _ ->
    let vars = env.ty in
    (vars, compile__type dkenv env ty)
