open Basic
open Ast
open Sttforall
open Environ

module CType = Compile_type
module CTerm = Compile_term
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
  ; strategy= Reduction.Whnf
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
    | App(Abs _, _te) -> [],`Beta(tyf')
    | App (f, a) ->
      let ctx, red = get_red f in
      (CAppL a)::ctx, red
    | Cst ((md, id), _tys) -> [], `Cst (of_name (mk_name (mk_mident md) (mk_ident id)), _tys)
    | _ -> assert false

  exception Equal

  let rec compare__term ctx left right =
    match (left, right) with
    | TeVar _, TeVar _ -> raise Equal
    | Cst (cst, _tys), Cst (cst', _tys') ->
      if cst = cst' then raise Equal
      else
        let name = name_of cst in
        if Env.is_static dloc name then
          (`Right, ctx, `Cst(cst', _tys'))
        else (`Left, ctx, `Cst(cst, _tys))
    | Abs (var, _ty, _tel), Abs (var', _, _ter) ->
      compare__term (CAbs(var,_ty)::ctx) _tel _ter
    | App(Abs _, _), _ -> (`Left, ctx, `Beta(left))
    | _, App(Abs _, _) -> (`Right, ctx, `Beta(right))
    | _, Cst (cst, _tys) -> (`Right, ctx, `Cst(cst,_tys))
    | Cst (cst, _tys), _ -> (`Left, ctx, `Cst(cst,_tys))
    | Forall (var, _ty, _tel), Forall (var', _, _ter) ->
      compare__term (CForall(var, _ty, _tel,_ter)::ctx) _tel _ter
    | Impl (_tel, _ter), Impl (_tel', _ter') -> (
        try compare__term (CImplL(_tel,_tel',_ter)::ctx) _tel _tel'
        with Equal ->  compare__term (CImplR(_tel,_ter,_ter')::ctx)_ter _ter')
    | App (_tel, _ter), App (_tel', _ter') -> (
        try
          let side, ctx, cst = compare__term (CAppL(_ter)::ctx) _tel _tel' in
          match cst with
          | `Cst(cst,_tys) ->
            if Env.is_static dloc (name_of cst) then
              if side = `Right then
                let ctxapp,red = get_red left in
                (`Left, ctxapp@ctx, red)
              else
                let ctxapp, red = get_red right in
                (`Right, ctxapp@ctx, red)
            else  (side, ctx, `Cst (cst, _tys))
          | _ ->  side, ctx, cst
        with Equal ->
        try compare__term (CAppR(_tel)::ctx) _ter _ter' with Failure _ ->
          let ctxapp, red = get_red _tel in
          (`Left, ctxapp@ctx, red)
          (* UGLY can be fixed by checking first the definability of a constant for the cst case *) )
    | _, App (_tel, _) ->
      let ctxapp,red = get_red right in
      (`Right, ctxapp@ctx, red)
    | App (_tel, _), _ ->
      let ctxapp,red = get_red left in
      (`Left, ctxapp@ctx, red)
    | _ -> failwith "todo2"

  let rec compare_term ctx left right =
    match (left, right) with
    | Te left, Te right -> compare__term ctx left right
    | ForallP (var, left), ForallP (var', right) ->
      compare_term (CForallP(var)::ctx) left right
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
        let cleft = CTerm.compile_term env left in
        let cleft' = CTerm.compile_term env left' in
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
      let right', trace' = annotate_beta env right in
      let trace_beta = {left=trace.left;right=trace'.left} in
      if Term.term_eq left' right' then trace_beta
      else
        let cleft = CTerm.compile_term env left' in
        let cright = CTerm.compile_term env right' in
        let side, ctx, red = compare_term [] cleft cright in
        let left'',right'' =
          match red with
          | `Cst((md,id), _tys) ->
            let cst = mk_name (mk_mident md) (mk_ident id) in
            let i = CTerm.get_type_arity env dloc cst in
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

let rec compile_proof env proof =
  match proof with
  | Term.DB (_, var, n) ->
      let var = get_dk_var env n in
      let te' = List.assoc var env.prf in
      let j = make_judgment env (TeSet.of_list env.prf) (Te te') in
      (j, Assume(j,var))
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env id in
      let jp, proof = compile_proof (CType.add_ty_var_dk env id) _te in
      let j = make_judgment env jp.hyp (ForallP (soi id, jp.thm)) in
      (j, ForallPI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _ty), _te)
    when is_sttfa_const sttfa_etap cst || is_sttfa_const sttfa_eta cst ->
      let _ty' = CType.compile_wrapped__type env _ty in
      let id = gen_fresh env id in
      let jp, proof = compile_proof (CTerm.add_te_var_dk env id _ty') _te in
      let j =
        make_judgment env jp.hyp
          (Te (Forall (soi id, _ty', extract_te jp.thm)))
      in
      (j, ForallI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _te), prf)
    when is_sttfa_const sttfa_eps cst ->
    let remove_hyp te = TeSet.filter (fun (id',_) ->
        if string_of_ident id=id' then false else true) in
    let _te' = CTerm.compile_wrapped__term env _te in
    let jp, proof = compile_proof (add_prf_ctx env (string_of_ident id) _te _te') prf in
    let j =
      make_judgment env (remove_hyp _te' jp.hyp)
        (Te (Impl (_te', extract_te jp.thm)))
    in
    (j, ImplI (j, proof, string_of_ident id))
  | Term.Const (lc, name) ->
      let te' =
        match Env.get_type lc name with
        | OK te -> CTerm.compile_wrapped_term env te
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

  let te' = CTerm.compile_wrapped_term env te in
  (*  Format.eprintf "left:%a@." Pp.print_term (Decompile.decompile_term env.dk j.thm); *)
  let j' = {j with thm = te'} in
  let j,f' = get_product env j f' in
(*  Format.eprintf "after:%a@." Pp.print_term (Decompile.decompile_term env.dk j.thm);
  Format.eprintf "fa:%a@." Pp.print_term (Decompile.decompile_term env.dk j'.thm); *)
  match j.thm with
  | ForallP _ ->
    let a' = CType.compile__type env a in
    (j', ForallPE(j', f',a'))
  | Te Forall _ ->
    let a' = CTerm.compile__term env a in
    let proof = ForallE(j', f',a') in
    let before = Decompile.decompile_term env.dk j'.thm in
    let after = Env.unsafe_reduction ~red:(beta_only) before in
    let trace = Tracer.annotate env before after in
    let thm' = CTerm.compile_term env after in
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
        let _te' = CTerm.compile__term env _te in
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
      | _,`Beta _ -> assert false
      | _,`Cst ((md,id),_tys) ->
        let cst = mk_name (mk_mident md) (mk_ident id) in
        Env.unsafe_reduction ~red:(delta_only cst) tyfl
        |> Env.unsafe_reduction ~red:(beta_only)
    in
    (*    Format.eprintf "right:%a@." Pp.print_term tyfr; *)
(*    Format.eprintf "left:%a@." Pp.print_term tyfl;
      Format.eprintf "right:%a@." Pp.print_term tyfr; *)
    let trace = Tracer.annotate env tyfl tyfr in
    let tyfr' = CTerm.compile__term env tyfr in
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
      let ty' = CType.compile_type empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      let ty' = CType.compile__type empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let te' = CTerm.compile_term empty_env a in
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
        (of_name name, CType.compile_type empty_env a, CTerm.compile_term empty_env term)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let j, proof = compile_proof empty_env term in
      let a' = CTerm.compile_term empty_env a in
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
      Theorem (of_name name, CTerm.compile_term empty_env a, proof')
  | _ -> assert false
