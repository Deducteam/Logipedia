open Basic
open Ast
open Sttforall

type proof_ctx = (ident * _te) list

type env =
  {k: int; dk: Term.typed_context; ty: ty_ctx; te: te_ctx; prf: proof_ctx}

let empty_env = {k= 0; dk= []; ty= []; te= []; prf= []}

let soi = string_of_ident

let rec gen_fresh ctx x c =
  let x' = if c < 0 then x else x ^ string_of_int c in
  if List.exists (fun (_, v, _) -> soi v = x') ctx then gen_fresh ctx x (c + 1)
  else mk_ident x'


let gen_fresh env x = gen_fresh env.dk (soi x) (-1)

let of_name name = (string_of_mident (md name), string_of_ident (id name))

let rec take i l =
  if i = 0 then []
  else match l with [] -> assert false | x :: l -> x :: take (i - 1) l


let rec drop i l =
  if i = 0 then l
  else match l with [] -> assert false | _ :: l -> drop (i - 1) l


let compile_tyop tyop =
  match tyop with Term.Const (_, name) -> of_name name | _ -> assert false


let get env n =
  let _, x, _ = List.nth env.dk n in
  soi x


let rec compile__type env _ty =
  match _ty with
  | Term.Const (_, cst) when is_sttfa_const sttfa_prop _ty -> Prop
  | Term.App (c, left, [right]) when is_sttfa_const sttfa_arrow c ->
      let left' = compile__type env left in
      let right' = compile__type env right in
      Arrow (left', right')
  | Term.DB (_, var, n) ->
      let var = get env n in
      TyVar var
  | Term.App (tyop, a, args) ->
      let tyop' = compile_tyop tyop in
      let args' = List.map (fun x -> compile__type env x) (a :: args) in
      TyOp (tyop', args')
  | Term.Const _ -> TyOp (compile_tyop _ty, [])
  | _ -> assert false


let add_ty_var env var =
  { env with
    k= env.k + 1
  ; ty= soi var :: env.ty
  ; dk=
      (dloc, var, Term.mk_Const dloc (mk_name sttfa_module sttfa_type))
      :: env.dk }


let rec compile_type (env: env) ty =
  match ty with
  | Term.App (c, Term.Lam (_, var, _, ty), [])
    when is_sttfa_const sttfa_forall_kind_type c ->
      let var = gen_fresh env var in
      let ty' = compile_type (add_ty_var env var) ty in
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


let add_te_var env var ty ty' =
  { env with
    k= env.k + 1; te= (soi var, ty') :: env.te; dk= (dloc, var, ty) :: env.dk
  }


let rec arity_of te =
  match te with ForallK (_, te) -> 1 + arity_of te | _ -> 0


let get_arity env lc name =
  match Env.get_type lc name with
  | OK ty -> arity_of (compile_wrapped_type env ty)
  | Err err -> Errors.fail_signature_error err


let rec compile__term env _te =
  match _te with
  | Term.DB (_, var, n) ->
      let var = get env n in
      TeVar var
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env id in
      let _te' = compile__term (add_ty_var env id) _te in
      AbsTy (soi id, _te')
  | Term.Lam (_, id, Some _ty, _te) ->
      let id = gen_fresh env id in
      let _ty' = compile_wrapped__type env _ty in
      let _te' = compile__term (add_te_var env id _ty _ty') _te in
      Abs (soi id, _ty', _te')
  | Term.App (cst, _ty, [(Term.Lam (_, id, Some _, _te))])
    when is_sttfa_const sttfa_forall cst ->
      let id = gen_fresh env id in
      let _ty' = compile__type env _ty in
      let _te' = compile__term (add_te_var env id _ty _ty') _te in
      Forall (soi id, _ty', _te')
  | Term.App (cst, tel, [ter]) when is_sttfa_const sttfa_impl cst ->
      let tel' = compile__term env tel in
      let ter' = compile__term env ter in
      Impl (tel', ter')
  | Term.App (Term.Const (lc, name), a, args) ->
      let cst' = of_name name in
      let i = get_arity env lc name in
      let args = a :: args in
      let ty_args, te_args = (take i args, drop i args) in
      let ty_args' = List.map (compile__type env) ty_args in
      let te_args' = List.map (fun x -> compile__term env x) te_args in
      List.fold_left
        (fun app arg -> App (app, arg))
        (Cst (cst', ty_args')) te_args'
  | Term.App (f, a, args) ->
      let f' = compile__term env f in
      let args' = List.map (fun x -> compile__term env x) (a :: args) in
      List.fold_left (fun app arg -> App (app, arg)) f' args'
  | Term.Lam (_, _, None, _) -> failwith "lambda untyped are not supported"
  | Term.Const (lc, cst) -> Cst (of_name cst, [])
  | _ ->
      Format.eprintf "%a@." Pp.print_term _te ;
      assert false


let rec compile_term env te =
  match te with
  | Term.App (cst, Term.Lam (_, x, Some ty, te), [])
    when is_sttfa_const sttfa_forall_kind_prop cst ->
      let x = gen_fresh env x in
      let te' = compile_term (add_ty_var env x) te in
      ForallP (soi x, te')
  | _ -> Te (compile__term env te)


let compile_wrapped_term env _te =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile_term env te
  | _ -> assert false


let compile_wrapped__term env _te =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile__term env te
  | _ -> assert false


let make_judgment env hyp thm = {ty= env.ty; te= env.te; hyp; thm}

let rec extract_te te = match te with Te _te -> _te | _ -> assert false

let beta_only : Reduction.red_cfg =
  let open Reduction in
  { nb_steps= None
  ; beta= true
  ; strategy= Reduction.Snf
  ; select=
      Some (fun name -> match name with Rule.Delta _ -> true | _ -> false) }


let debug : Reduction.red_cfg =
  Reduction.{default_cfg with select= Some (fun _ -> true)}


let add_prf_ctx env id _te _te' =
  { env with
    k= env.k + 1
  ; prf= (id, _te') :: env.prf
  ; dk= (Basic.dloc, id, _te) :: env.dk }


let rec compile_proof env proof =
  match proof with
  | Term.DB (_, var, n) ->
      let var = get env n in
      let te' = List.assoc (mk_ident var) env.prf in
      let j = make_judgment env (TeSet.singleton te') (Te te') in
      (j, Assume j)
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env id in
      let jp, proof = compile_proof (add_ty_var env id) _te in
      let j = make_judgment env jp.hyp (ForallP (soi id, jp.thm)) in
      (j, ForallPI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _ty), _te)
    when is_sttfa_const sttfa_etap cst || is_sttfa_const sttfa_eta cst ->
      let _ty' = compile_wrapped__type env _ty in
      let id = gen_fresh env id in
      let jp, proof = compile_proof (add_te_var env id _ty _ty') _te in
      let j =
        make_judgment env jp.hyp
          (Te (Forall (soi id, _ty', extract_te jp.thm)))
      in
      (j, ForallI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _te), prf)
    when is_sttfa_const sttfa_eps cst ->
      let _te' = compile_wrapped__term env _te in
      let jp, proof = compile_proof (add_prf_ctx env id _te _te') prf in
      let j =
        make_judgment env (TeSet.remove _te' jp.hyp)
          (Te (Impl (_te', extract_te jp.thm)))
      in
      (j, ImplI (j, proof))
  | Term.Const (lc, name) ->
      let te' =
        match Env.get_type lc name with
        | OK te -> compile_wrapped_term env te
        | Err err -> assert false
      in
      let j = make_judgment env TeSet.empty te' in
      (j, Lemma (of_name name, j))
  | Term.App (f, a, args) ->
      let f' = compile_proof env f in
      snd
      @@ List.fold_left
           (fun (f, f') a -> compile_args env f f' a)
           (f, f') (a :: args)
  | _ ->
      Format.eprintf "%a@." Pp.print_term proof ;
      failwith "todo"


and compile_args env f f' arg =
  let thmf, f' = f' in
  let tyf =
    match Env.infer ~ctx:env.dk f with
    | OK ty -> Env.unsafe_reduction ~red:beta_only ty
    | Err err -> Errors.fail_env_error err
  in
  let tyf' = compile_wrapped_term env tyf in
  let fa = Term.mk_App f arg [] in
  let te =
    match Env.infer ~ctx:env.dk fa with
    | OK te -> Env.unsafe_reduction ~red:beta_only te
    | Err err -> Errors.fail_env_error err
  in
  let te' = compile_wrapped_term env te in
  let j = {thmf with thm= te'} in
  let f' = if tyf' = thmf.thm then f' else Conv ({thmf with thm= tyf'}, f') in
  match tyf' with
  | ForallP _ ->
      let arg = compile__type env arg in
      (fa, (j, ForallPE (j, f', arg)))
  | Te Forall _ ->
      let arg = compile__term env arg in
      (fa, (j, ForallE (j, f', arg)))
  | Te Impl _ ->
      let j', arg' = compile_proof env arg in
      let j = {j with hyp= TeSet.union j.hyp j'.hyp} in
      (fa, (j, ImplE (j, f', arg')))
  | _ ->
      Format.eprintf "%a@." Pp.print_term f ;
      assert false


let compile_declaration name ty =
  Format.eprintf "Compile %a@." pp_name name ;
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      let ty' = compile_type empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      let ty' = compile__type empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let te' = compile_term empty_env a in
      Axiom (of_name name, te')
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty ->
      TyOpDef (of_name name, 0)
  | _ ->
      Format.eprintf "%a@." Pp.print_term ty ;
      assert false


let compile_definition name ty term =
  Format.eprintf "Compile %a@." pp_name name ;
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      Definition
        (of_name name, compile_type empty_env a, compile_term empty_env term)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      Theorem
        ( of_name name
        , compile_term empty_env a
        , snd @@ compile_proof empty_env term )
  | _ -> assert false
