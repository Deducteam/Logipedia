open Basic
open Ast
open Sttforall

type proof_ctx = (string * _te) list

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
    let old = var in
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


let rec type_arity_of te =
  match te with ForallK (_, te) -> 1 + type_arity_of te | _ -> 0


let get_type_arity env lc name =
  match Env.get_type lc name with
  | OK ty -> type_arity_of (compile_wrapped_type env ty)
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
    let i = get_type_arity env lc name in
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
  | _ ->  Te (compile__term env te)


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

let beta_one =
  let open Reduction in
  { nb_steps= Some 1
  ; beta= true
  ; strategy= Reduction.Hnf
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
  ; beta = false
  ; strategy= Reduction.Snf
  ; select=
      Some
        (fun name ->
          match name with
          | Delta cst' -> if name_eq cst' cst then true else false
          | _ -> false ) }


module Tracer = struct
  let rec get_red tyf' =
    match tyf' with
    | App(Abs _, _) -> `Beta(tyf')
    | App (f, a) -> get_red f
    | Cst ((md, id), _tys) -> `Cst (of_name (mk_name (mk_mident md) (mk_ident id)), _tys)
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

  let to_name (md, id) = mk_name (mk_mident md) (mk_ident id)

  let rec compare__term ctx left right =
    match (left, right) with
    | TeVar _, TeVar _ -> raise Equal
    | Cst (cst, _tys), Cst (cst', _tys') ->
      if cst = cst' then raise Equal
      else
        let name = to_name cst in
        if Env.is_static dloc name then
          (`Right, ctx, `Cst(cst', _tys'))
        else (`Left, ctx, `Cst(cst, _tys))
    | Abs (_, _, _tel), Abs (_, _, _ter) -> compare__term (CAbs::ctx) _tel _ter
    | App(Abs _, _), _ -> (`Left, ctx, `Beta(left))
    | _, App(Abs _, _) -> (`Right, ctx, `Beta(right))
    | _, Cst (cst, _tys) -> (`Right, ctx, `Cst(cst,_tys))
    | Cst (cst, _tys), _ -> (`Left, ctx, `Cst(cst,_tys))
    | Forall (_, _, _tel), Forall (_, _, _ter) -> compare__term (CForall::ctx) _tel _ter
    | Impl (_tel, _ter), Impl (_tel', _ter') -> (
        try compare__term (CImplL::ctx) _tel _tel'
        with Equal ->  compare__term (CImplR::ctx)_ter _ter')
    | App (_tel, _ter), App (_tel', _ter') -> (
        try
          let side, ctx, cst = compare__term (CAppL::ctx) _tel _tel' in
          match cst with
          | `Cst(cst,_tys) ->
            if Env.is_static dloc (to_name cst) then
              if side = `Right then (`Left, ctx, get_red _tel)
              else (`Right, ctx, get_red _tel')
            else  (side, ctx, `Cst (cst, _tys))
          | _ ->  side, ctx, cst
        with Equal ->
        try compare__term (CAppR::ctx) _ter _ter' with Failure _ ->
          (`Left, ctx, get_red _tel)
          (* UGLY can be fixed by checking first the definability of a constant for the cst case *) )
    | _, App (_tel, _) -> (`Right, ctx, get_red _tel)
    | App (_tel, _), _ -> (`Left, ctx, get_red _tel)
    | _ -> failwith "todo2"

  let rec compare_term ctx left right =
    match (left, right) with
    | Te left, Te right -> compare__term ctx left right
    | ForallP (_, left), ForallP (_, right) -> compare_term (CForallP::ctx) left right
    | _ -> assert false

  let reduce_delta env term cst i =
    let term' = Env.unsafe_reduction ~red:(delta_only cst) term in
    Env.unsafe_reduction ~red:(beta_only_n i) term'

  let rec annotate_beta env left =
    let left' = Env.unsafe_reduction ~red:(beta_only_n 1) left in
    if left = left' then
      left, {left = [] ; right = [] }
    else
      begin
        let cleft = compile_term env left in
        let cleft' = compile_term env left' in
(*        Format.eprintf "before: %a@." Pp.print_term left;
        Format.eprintf "after: %a@." Pp.print_term left'; *)
        let _,ctx,red = compare_term [] cleft cleft' in
        let t = match red with
          | `Beta t -> t
          | `Cst((md,id),_) -> Format.eprintf "%s,%s@." md id ; assert false in
        let left'',trace = annotate_beta env left' in
        left'', {trace with left= (Beta t, ctx) :: trace.left}
      end

  let rec annotate env left right =
    let add_delta side ctx (cst,_tys) trace =
      if side = `Right then {trace with right= (Delta(cst,_tys), ctx) :: trace.right}
      else if side = `Left then {trace with left= (Delta(cst,_tys), ctx) :: trace.left}
      else assert false
    in
    if Term.term_eq left right then { left = [] ; right = [] }
    else
      let left',trace = annotate_beta env left in
      let right', trace = annotate_beta env right in
      let trace_beta = {left=trace.left;right=trace.left} in
      if Term.term_eq left' right' then trace_beta
      else
        let cleft = compile_term env left' in
        let cright = compile_term env right' in
        let side, ctx, red = compare_term [] cleft cright in
        let left'',right'' =
          match red with
          | `Cst((md,id), _tys) ->
            let cst = mk_name (mk_mident md) (mk_ident id) in
            let i = get_type_arity env dloc cst in
            if side = `Right then
              (left', reduce_delta env right' cst i)
            else if side = `Left then
              (reduce_delta env left' cst i, right')
            else assert false
          | _ -> (
              Format.eprintf "before: %a@." Pp.print_term left';
              Format.eprintf "after: %a@." Pp.print_term right';
              assert false )
        in
        let trace = annotate env left'' right'' in
        let trace =
          match red with
          | `Cst((md,id),_tys) ->
            add_delta side ctx ((md, id),_tys) trace
          | _ -> assert false
        in
        {left=trace_beta.right@trace.left; right = trace_beta.right@trace.right}

end

let debug : Reduction.red_cfg =
  Reduction.{default_cfg with select= Some (fun _ -> true)}


let add_prf_ctx env id _te _te' =
  { env with
    k= env.k + 1
  ; prf= (id, _te') :: env.prf
  ; dk= (Basic.dloc, mk_ident id, _te) :: env.dk }


let rec compile_proof env proof =
  match proof with
  | Term.DB (_, var, n) ->
      let var = get env n in
      let te' = List.assoc var env.prf in
      let j = make_judgment env (TeSet.of_list env.prf) (Te te') in
      (j, Assume(j,var))
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
    let remove_hyp te = TeSet.filter (fun (id',_) ->
        if string_of_ident id=id' then false else true) in
    let _te' = compile_wrapped__term env _te in
    let jp, proof = compile_proof (add_prf_ctx env (string_of_ident id) _te _te') prf in
    let j =
      make_judgment env (remove_hyp _te' jp.hyp)
        (Te (Impl (_te', extract_te jp.thm)))
    in
    (j, ImplI (j, proof, string_of_ident id))
  | Term.Const (lc, name) ->
      let te' =
        match Env.get_type lc name with
        | OK te -> compile_wrapped_term env te
        | Err err -> assert false
      in
      let j = make_judgment env (TeSet.of_list env.prf) te' in
      (j, Lemma (of_name name, j))
  | Term.App (f, a, args) ->
    let j,f' = compile_proof env f in
    List.fold_left (fun (j,f') a -> compile_arg env j f' a) (j,f') (a::args)
  | _ -> assert false

and compile_arg env j f' a =
  let f = Decompile.decompile_proof env.dk f' in
  let fa = Term.mk_App f a [] in
  let te =
    match Env.infer ~ctx:env.dk fa with
    | OK te ->
      (* Format.eprintf "teb:%a@." Pp.print_term te; *)
      let te' = Env.unsafe_reduction ~red:(beta_only_n 1) te (* administrative beta *) in
      (*      Format.eprintf "eq:%b@." (Term.term_eq te te'); *)
      te'
    | Err err -> Errors.fail_env_error err
  in
(*  Format.eprintf "tea:%a@." Pp.print_term te; *)

  let te' = compile_wrapped_term env te in
  (*  Format.eprintf "left:%a@." Pp.print_term (Decompile.decompile_term env.dk j.thm); *)
  let j' = {j with thm = te'} in
  let j,f' = get_product env j f' in
(*  Format.eprintf "after:%a@." Pp.print_term (Decompile.decompile_term env.dk j.thm);
  Format.eprintf "fa:%a@." Pp.print_term (Decompile.decompile_term env.dk j'.thm); *)
  match j.thm with
  | ForallP _ ->
    let a' = compile__type env a in
    (j', ForallPE(j', f',a'))
  | Te Forall _ ->
    let a' = compile__term env a in
    let proof = ForallE(j', f',a') in
    let before = Decompile.decompile_term env.dk j'.thm in
    let after = Env.unsafe_reduction ~red:(beta_only) before in
    let trace = Tracer.annotate env before after in
    let thm' = compile_term env after in
    let j' = {j with thm = thm'} in
    (j', Conv(j', proof, trace))
  | Te Impl(p',q') ->
    let ja,a' = compile_proof env a in
    let _te = match ja.thm with Te _te -> _te | _ -> assert false in
    let pq = Decompile.decompile__term env.dk (Impl(p',q')) in
    let _te = Decompile.decompile__term env.dk (Impl(_te,q')) in
    if Term.term_eq pq _te then
      (j', ImplE(j', f',a'))
    else
      begin
(*        Format.eprintf "left:%a@." Pp.print_term pq;
          Format.eprintf "right:%a@." Pp.print_term _te; *)
        let trace = Tracer.annotate env pq _te in
        let _te' = compile__term env _te in
        let f' = Conv({j with thm = Te _te'}, f', trace) in
        (j', ImplE(j',f',a'))
      end
  | Te tyfl' -> assert false

and get_product env j f' =
  match j.thm with
  | ForallP _ | Te Forall _ | Te Impl _ -> (j,f')
  | Te tyfl' ->
    let tyfl = Decompile.decompile__term env.dk tyfl' in
    let tyfr =
      match Tracer.get_red tyfl' with
      | `Beta _ -> assert false
      | `Cst ((md,id),_tys) ->
        let cst = mk_name (mk_mident md) (mk_ident id) in
        Env.unsafe_reduction ~red:(delta_only cst) tyfl
        |> Env.unsafe_reduction ~red:(beta_only)
    in
    (*    Format.eprintf "right:%a@." Pp.print_term tyfr; *)
(*    Format.eprintf "left:%a@." Pp.print_term tyfl;
      Format.eprintf "right:%a@." Pp.print_term tyfr; *)
    let trace = Tracer.annotate env tyfl tyfr in
    let tyfr' = compile__term env tyfr in
    let j' = {j with thm = Te tyfr'} in
    let proof' = Conv(j',f',trace) in
    get_product env j' proof'
(*
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
    | OK te -> Env.unsafe_reduction ~red:beta_only te
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
      (fa, (j, Conv(j,ForallE (j, f', arg), {left=[Beta];right=[]})))
  | Te Impl (l, _) ->
      let j', arg' = compile_proof env arg in
      let a' = match j'.thm with Te a -> a | _ -> assert false in
      let b' = match j.thm with Te a -> a | _ -> assert false in
      let thmf' = {thmf with thm= Te (Impl (a', b'))} in
      let trace =
        Tracer.annotate env
          (Decompile.decompile_term env.dk tyf')
          (Decompile.decompile_term env.dk (Te (Impl (a', b'))))
      in
      let f' = Conv (thmf', f', trace) in
      let j'' = {j with hyp = TeSet.union j'.hyp j.hyp} in
      (fa, (j'', ImplE (j'', f', arg')))
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
*)
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
(*        Format.eprintf "left: %a@." Pp.print_term (Decompile.decompile_term [] j.thm);
          Format.eprintf "right: %a@." Pp.print_term (Decompile.decompile_term [] a'); *)
        if j.thm = a' then proof
        else
          Conv
            ( {j with thm=a'}
            , proof
            , Tracer.annotate empty_env (Decompile.decompile_term [] j.thm) a
            )
      in
      Theorem (of_name name, compile_term empty_env a, proof')
  | _ -> assert false
