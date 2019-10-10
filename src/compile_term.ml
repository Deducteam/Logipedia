open Ast
open Sttfadk
open Environ

module CType = Compile_type

module T = Kernel.Term
module Env = Api.Env
module Err = Api.Errors
module Pp = Api.Pp.Default

let rec type_arity_of te =
  match te with ForallK (_, te) -> 1 + type_arity_of te | _ -> 0

let get_type_arity dkenv env lc name =
  type_arity_of @@
  CType.compile_wrapped_type dkenv env (Env.get_type dkenv lc name)


let compile__term (dkenv:Env.t) env _te =
  let rec compile__term env _te =
    match _te with
    | T.DB (_, _, n) ->
      let var = get_dk_var env n in
      TeVar var
    | T.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env id in
      let _te' = compile__term (add_ty_var_dk env id) _te in
      AbsTy (soi id, _te')
    | T.Lam (_, id, Some _ty, _te) ->
      let id = gen_fresh env id in
      let _ty' = CType.compile_wrapped__type dkenv env _ty in
      let _te' = compile__term (add_te_var_dk env id _ty') _te in
      Abs (soi id, _ty', _te')
    | T.App (cst, _ty, [(T.Lam (_, id, Some _, _te))])
      when is_sttfa_const sttfa_forall cst ->
      let id = gen_fresh env id in
      let _ty' = CType.compile__type dkenv env _ty in
      let _te' = compile__term (add_te_var_dk env id _ty') _te in
      Forall (soi id, _ty', _te')
    | T.App (cst, tel, [ter]) when is_sttfa_const sttfa_impl cst ->
      let tel' = compile__term env tel in
      let ter' = compile__term env ter in
      Impl (tel', ter')
    | T.App (T.Const (lc, name), a, args) ->
      let cst' = of_name name in
      let i = get_type_arity dkenv env lc name in
      let args = a :: args in
      let ty_args, te_args = (take i args, drop i args) in
      let ty_args' = List.map (CType.compile__type dkenv env) ty_args in
      let te_args' = List.map (fun x -> compile__term env x) te_args in
      List.fold_left
        (fun app arg -> App (app, arg))
        (Cst (cst', ty_args')) te_args'
    | T.App (f, a, args) ->
      let f' = compile__term env f in
      let args' = List.map (fun x -> compile__term env x) (a :: args) in
      List.fold_left (fun app arg -> App (app, arg)) f' args'
    | T.Lam (_, _, None, _) -> failwith "lambda untyped are not supported"
    | T.Const (_, cst) -> Cst (of_name cst, [])
    | _ ->
      Format.eprintf "%a@." Pp.print_term _te ;
      assert false
  in
  compile__term env _te

let rec compile_term (dkenv:Env.t) env te =
  match te with
  | T.App (cst, T.Lam (_, x, Some _, te), [])
    when is_sttfa_const sttfa_forall_kind_prop cst ->
    let x = gen_fresh env x in
    let te' = compile_term dkenv (add_ty_var_dk env x) te in
    ForallP (soi x, te')
  | _ ->  Te (compile__term dkenv env te)


let compile_wrapped_term dkenv env _te =
  match _te with
  | T.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile_term dkenv env te
  | _ -> assert false


let compile_wrapped__term dkenv env _te =
  match _te with
  | T.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile__term dkenv env te
  | _ -> Format.eprintf "%a@." Pp.print_term _te; assert false
