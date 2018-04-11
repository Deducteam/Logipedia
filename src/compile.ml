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
  ; select= Some (fun _ -> false) }


let beta_only_n : int -> Reduction.red_cfg =
 fun i ->
  let open Reduction in
  { nb_steps= Some i
  ; beta= true
  ; strategy= Reduction.Snf
  ; select= Some (fun _ -> false) }


let beta_delta_only : Reduction.red_cfg =
  let open Reduction in
  let open Rule in
  { nb_steps= None
  ; beta= true
  ; strategy= Reduction.Snf
  ; select= Some (fun cst -> match cst with Delta _ -> true | _ -> false) }


let delta_only : Basic.name -> Reduction.red_cfg =
 fun cst ->
  let open Reduction in
  let open Rule in
  { nb_steps= Some 1
  ; beta= true
  ; strategy= Reduction.Snf
  ; select=
      Some
        (fun name ->
          match name with
          | Delta cst' -> if name_eq cst' cst then true else false
          | _ -> false ) }


module Tracer = struct
  let rec get_cst tyf' =
    match tyf' with
    | App (f, a) -> get_cst f
    | Cst ((md, id), _tyl) -> mk_name (mk_mident md) (mk_ident id)
    | _ -> assert false


  let rec equal__term map left right =
    match (left, right) with
    | TeVar var, TeVar var' -> List.assoc var map = var'
    | Abs (tevar, _ty, _tel), Abs (tevar', _ty', _ter) ->
        equal__term ((tevar, tevar') :: map) _tel _ter
    | App (f, a), App (f', a') -> equal__term map f f' && equal__term map a a'
    | Forall (tevar, _ty, _tel), Forall (tevar', _ty', _ter) ->
        equal__term ((tevar, tevar') :: map) _tel _ter
    | Impl (f, a), Impl (f', a') ->
        equal__term map f f' && equal__term map a a'
    | AbsTy (_, _tel), AbsTy (_, _ter) -> equal__term map _tel _ter
    | Cst (cst, _), Cst (cst', _) -> cst = cst'
    | _ -> false


  let rec equal_term map left right =
    match (left, right) with
    | Te tel, Te ter -> equal__term map tel ter
    | ForallP (var, tel), ForallP (var', ter) ->
        equal_term ((var, var') :: map) tel ter
    | _ -> false


  exception Equal

  let rec compare__term left right =
    match (left, right) with
    | TeVar _, TeVar _ -> raise Equal
    | Cst (cst, _), Cst (cst', _) ->
        if cst = cst' then raise Equal else failwith "todo3"
    | _, Cst (cst, _) -> (`Right, cst)
    | Forall (_, _, _tel), Forall (_, _, _ter) -> compare__term _tel _ter
    | Impl (_tel, _ter), Impl (_tel', _ter') -> (
      try compare__term _tel _tel' with Equal -> compare__term _ter _ter' )
    | App (_tel, _ter), App (_tel', _ter') -> (
      try compare__term _tel _tel' with Equal -> compare__term _ter _ter' )
    | _, App (_tel, _) -> (`Right, of_name (get_cst _tel))
    | App (_tel, _), _ -> (`Left, of_name (get_cst _tel))
    | _ ->
        Printf.printf "l:%a\nr:%a\n" Print.print__te left Print.print__te right ;
        failwith "todo2"


  let rec compare_term left right =
    match (left, right) with
    | Te left, Te right -> compare__term left right
    | ForallP (_, left), ForallP (_, right) -> compare_term left right
    | _ -> failwith "todo1"


  let rec annotate left right =
    let add_beta trace =
      {left= Beta :: trace.left; right= Beta :: trace.right}
    in
    let add_delta side cst trace =
      if side = `Right then {trace with right= Delta cst :: trace.right}
      else if side = `Left then {trace with left= Delta cst :: trace.left}
      else assert false
    in
    let left = Env.unsafe_reduction ~red:beta_only right in
    let right = Env.unsafe_reduction ~red:beta_only right in
    if Term.term_eq left right then {left= [Beta]; right= [Beta]}
    else
      let left' = compile_term empty_env left in
      let right' = compile_term empty_env right in
      let side, (md, id) = compare_term left' right' in
      let cst = mk_name (mk_mident md) (mk_ident id) in
      let left, right =
        if side = `Right then
          (left, Env.unsafe_reduction ~red:(delta_only cst) right)
        else if side = `Left then
          let left = Env.unsafe_reduction ~red:(delta_only cst) left in
          (left, right)
        else assert false
      in
      let trace = annotate left right in
      add_delta side (md, id) (add_beta trace)
end

(*      Some (fun name -> match name with Rule.Delta _ -> true | _ -> false) } *)

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
           (fun (f, f') a -> compile_arg env f f' a)
           (f, f') (a :: args)
  | _ -> assert false


and compile_arg env f (j, f') arg =
  let tyf =
    match Env.infer ~ctx:env.dk f with
    | OK ty -> ty
    | Err err -> Errors.fail_env_error err
  in
  compile_args_aux env f tyf j f' arg


and compile_args_aux env f tyf thmf f' arg =
  let tyf = Env.unsafe_reduction ~red:beta_only tyf in
  let tyf' = compile_wrapped_term env tyf in
  let fa = Term.mk_App f arg [] in
  let te =
    match Env.infer ~ctx:env.dk fa with
    | OK te -> Env.unsafe_reduction ~red:beta_delta_only te
    | Err err -> Errors.fail_env_error err
  in
  let te' = compile_wrapped_term env te in
  let j = {thmf with thm= te'} in
  let f' =
    if tyf' = thmf.thm then f'
    else Conv ({thmf with thm= tyf'}, f', {left= [Beta]; right= []})
  in
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
  | Te tyf' ->
      let cst = Tracer.get_cst tyf' in
      let i = get_arity env dloc cst in
      let tyf = Env.unsafe_reduction ~red:(delta_only cst) tyf in
      let tyf = Env.unsafe_reduction ~red:(beta_only_n i) tyf in
      let tyf' = compile_wrapped_term env tyf in
      let f' =
        Conv
          ({thmf with thm= tyf'}, f', {left= [Delta (of_name cst)]; right= []})
      in
      compile_args_aux env f tyf thmf f' arg


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
      let j, proof = compile_proof empty_env term in
      let a' = compile_term empty_env a in
      let proof' =
        if j.thm = a' then proof
        else
          Conv
            (j, proof, Tracer.annotate (Decompile.decompile_term 0 [] j.thm) a)
      in
      Theorem (of_name name, compile_term empty_env a, proof')
  | _ -> assert false
