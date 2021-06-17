open Ast
open Sttfadk
open Environ

module CType = Compile_type

module Env = Api.Env
module Dpp = Api.Pp.Default

let rec type_arity_of te =
  match te with ForallK (_, te) -> 1 + type_arity_of te | _ -> 0

let get_type_arity denv env lc name =
  try type_arity_of (CType.compile_wrapped_type denv env (Env.get_type denv lc name))
  with e -> Env.fail_env_error denv Kernel.Basic.dloc e


let rec compile__term denv env _te =
  match _te with
  | Term.DB (_, _, n) ->
      let var = get_dk_var env n in
      TeVar var
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
    let id = gen_fresh env [] id in
      let _te' = compile__term denv (add_ty_var_dk env id) _te in
      AbsTy (soi id, _te')
  | Term.Lam (_, id, Some _ty, _te) ->
      let id = gen_fresh env [] id in
      let _ty' = CType.compile_wrapped__type denv env _ty in
      let _te' = compile__term denv (add_te_var_dk env id _ty') _te in
      Abs (soi id, _ty', _te')
  | Term.App (cst, _ty, [(Term.Lam (_, id, Some _, _te))])
    when is_sttfa_const sttfa_forall cst ->
      let id = gen_fresh env [] id in
      let _ty' = CType.compile__type denv env _ty in
      let _te' = compile__term denv (add_te_var_dk env id _ty') _te in
      Forall (soi id, _ty', _te')
  | Term.App (cst, tel, [ter]) when is_sttfa_const sttfa_impl cst ->
    let tel' = compile__term denv env tel in
    let ter' = compile__term denv env ter in
    Impl (tel', ter')
  | Term.App (Term.Const (lc, name), a, args) ->
    let cst' = of_name name in
    let i = get_type_arity denv env lc name in
    let args = a :: args in
    let ty_args, te_args = (take i args, drop i args) in
    let ty_args' = List.map (CType.compile__type denv env) ty_args in
    let te_args' = List.map (compile__term denv env) te_args in
    List.fold_left
      (fun app arg -> App (app, arg))
      (Cst (cst', ty_args')) te_args'
  | Term.App (f, a, args) ->
    let f' = compile__term denv env f in
      let args' = List.map (fun x -> compile__term denv env x) (a :: args) in
      List.fold_left (fun app arg -> App (app, arg)) f' args'
  | Term.Lam (_, _, None, _) -> failwith "lambda untyped are not supported"
  | Term.Const (_, cst) -> Cst (of_name cst, [])
  | _ ->
      Format.eprintf "%a@." Dpp.print_term _te ;
      assert false

let rec compile_term denv env te =
  match te with
  | Term.App (cst, Term.Lam (_, x, Some _, te), [])
    when is_sttfa_const sttfa_forall_kind_prop cst ->
    let x = gen_fresh env [] x in
    let te' = compile_term denv (add_ty_var_dk env x) te in
    ForallP (soi x, te')
  | _ ->  Te (compile__term denv env te)


let compile_wrapped_term denv env _te =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile_term denv env te
  | _ -> assert false


let compile_wrapped__term denv env _te =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile__term denv env te
  | _ -> Format.eprintf "%a@." Dpp.print_term _te; assert false
