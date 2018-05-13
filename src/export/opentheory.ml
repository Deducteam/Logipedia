open Ast
open Compile
open Openstt
open Environ

let cur_md = ref ""

let sanitize id = id

let mk_id id = mk_name [] (sanitize id)

let mk_qid (md,id) = mk_name [md] id

let rec mk__ty = function
  | TyVar(var) -> mk_varType (mk_id var)
  | Arrow(_tyl,_tyr) ->
    let _tys' = List.map mk__ty [_tyl;_tyr] in
    ty_of_tyOp (mk_tyOp (mk_id "->")) _tys'
  | TyOp(tyop, _tys) ->
    let _tys' = List.map mk__ty _tys in
    ty_of_tyOp  (mk_tyOp (mk_qid tyop))  _tys'
  | Prop ->
    ty_of_tyOp (mk_tyOp (mk_id "bool")) []

let rec mk_ty = function
  | ForallK(var, ty) ->
    mk_ty ty
  | Ty(_ty) -> mk__ty _ty

let beta_only : Reduction.red_cfg =
  let open Reduction in
  { nb_steps= None
  ; beta= true
  ; strategy= Reduction.Snf
  ; select= Some (fun _ -> false) }

let rec mk__te ctx = function
  | TeVar(var) ->
    let _ty = List.assoc var ctx.te in
    let _ty' = mk__ty _ty in
    mk_var_term (mk_var (mk_id var) _ty')
  | Abs(var,_ty,_te) ->
    let ctx' = add_te_var ctx var _ty in
    let _ty' = mk__ty _ty in
    let var' = mk_var (mk_id var) _ty' in
    let _te' = mk__te ctx' _te in
    mk_abs_term var' _te'
  | App(_tel,_ter) ->
    let _tel' = mk__te ctx _tel in
    let _ter' = mk__te ctx _ter in
    mk_app_term _tel' _ter'
  | Forall(var,_ty,_te) ->
    let _ty' = mk__ty _ty in
    let f' = mk__te ctx (Abs(var,_ty,_te)) in
    mk_forall_term f' _ty'
  | Impl(_tel,_ter) ->
    let _tel' = mk__te ctx _tel in
    let _ter' = mk__te ctx _ter in
    mk_impl_term _tel' _ter'
  | AbsTy(var, _te) ->
    let ctx' = add_ty_var ctx var in
    mk__te ctx' _te
  | Cst(cst, _tys) ->
    let open Basic in
    let name = name_of cst in
    let _tys' = List.map (Decompile.decompile__type ctx.dk) _tys in
    let cst' =
      match _tys' with
      | [] -> Term.mk_Const dloc name
      | x::t -> Term.mk_App (Term.mk_Const dloc name) x t
    in
    Pp.print_db_enabled := true;
    match Env.infer ~ctx:ctx.dk cst' with
    | OK _ty ->
      let _ty' = CType.compile_wrapped__type ctx (Env.unsafe_reduction ~red:beta_only _ty) in
      term_of_const (const_of_name (mk_qid cst)) (mk__ty _ty')
    | Err err -> Errors.fail_env_error err

let rec mk_te ctx = function
  | ForallP(var,te) ->
    let ctx' = add_ty_var ctx var in
    mk_te ctx' te
  | Te(_te) -> mk__te ctx _te

let judgment_of = function
  | Assume(j,_)     -> j
  | Lemma(_,j)      -> j
  | Conv(j,_,_)     -> j
  | ImplE(j,_,_)    -> j
  | ImplI(j,_,_)    -> j
  | ForallE(j,_,_)  -> j
  | ForallI(j,_,_)  -> j
  | ForallPE(j,_,_) -> j
  | ForallPI(j,_,_) -> j

let add_prf_ctx env id _te _te' =
  { env with
    k= env.k + 1
  ; prf= (id, _te') :: env.prf
  ; dk= (Basic.dloc, Basic.mk_ident id, _te) :: env.dk }


let print_rewrite oc r =
  match r with
  | Delta((md,id),_tys) -> Format.fprintf oc "%s,%s" md id
  | Beta _ -> Format.fprintf oc "beta"

let rec get_vars = function
  | Ty _ -> []
  | ForallK(var, ty) -> var::(get_vars ty)

let mk_rewrite ctx r =
  let open Basic in
  match r with
  | Beta(t) ->
    let t' = mk__te ctx t in
    mk_betaConv t'
  | Delta((md,id),_tys) ->
    let cst = mk_name (mk_mident md) (mk_ident id) in
    let ty =
      match Env.get_type dloc cst with
      | OK ty -> ty
      | Err _ -> assert false
    in
    let ty' = CType.compile_type ctx ty in
    let vars = get_vars ty' in
    assert (List.length vars = List.length _tys);
    let vars' = List.map mk_id vars in
    let _tys' = List.map mk__ty _tys in
    let thm = thm_of_const_name (mk_qid (md,id)) in
    mk_subst thm (List.combine vars' _tys') []

let print_ctx oc = function
  | CAbs _     -> Format.fprintf oc "CAbs"
  | CAppL _    -> Format.fprintf oc "CAppL"
  | CAppR _    -> Format.fprintf oc "CAppR"
  | CForall _  -> Format.fprintf oc "CForall"
  | CImplL _   -> Format.fprintf oc "CImplL"
  | CImplR _   -> Format.fprintf oc "CImplR"
  | CAbsTy _   -> Format.fprintf oc "CAbsTy"
  | CForallP _ -> Format.fprintf oc "CForallP"

let print_ctxs oc ctxs =
  Basic.pp_list "," print_ctx oc (List.rev ctxs)

let print_rewrite_ctx oc (rw,ctxs) =
  Format.fprintf oc "unfold %a at %a;@." print_rewrite rw print_ctxs ctxs

let print_rewrite_seq oc rws = List.iter (print_rewrite_ctx oc) rws

let print_trace oc trace =
  Format.fprintf oc "left:@.%a@." print_rewrite_seq trace.left;
  Format.fprintf oc "right:@.%a@." print_rewrite_seq trace.right

let mk_beta env _te =
  let _te' = mk__te env _te in
  mk_betaConv _te'

let mk_delta ctx name _tys =
  let open Basic in
  let thm = thm_of_const_name (mk_qid name) in
  let term =
    Term.mk_Const dloc (mk_name (mk_mident (fst name)) (mk_ident (snd name)))
  in
  let ty =
    match Env.infer ~ctx:ctx.dk term  with
    | OK ty -> ty
    | Err err -> assert false
  in
  let ty' = CType.compile_wrapped_type ctx ty in
  let vars = get_vars ty' in
  let vars' = List.map mk_id vars in
  let _tys' = List.map mk__ty _tys in
  assert (List.length vars = List.length _tys);
  let subst = List.combine vars' _tys' in
  mk_subst thm subst []

  let env_of_ctx env =
    function
    | CAppL _ | CAppR _ | CImplL _ | CImplR _ -> env
    | CAbs(var,_ty) ->
      add_te_var env var _ty
    | CForall(var,_ty,_,_) ->
      add_te_var env var _ty
    | CAbsTy(var) ->
      add_ty_var env var
    | CForallP(var) ->
      add_ty_var env var

let mk_ctx env thm ctx =
  match ctx with
  | CAbs(var,_ty) ->
    let id = mk_id var in
    let _ty' = mk__ty _ty in
    let var = mk_var id _ty' in
    mk_absThm var thm
  | CAppL(_te) ->
    let _te' = mk__te env _te in
    mk_appThm thm (mk_refl _te')
  | CAppR(_te) ->
    let _te' = mk__te env _te in
    mk_appThm (mk_refl _te') thm
  | CForall(var,_ty, _tel, _ter) ->
    let id = mk_id var in
    let _ty' = mk__ty _ty in
    let _tel' = mk__te env _tel in
    let _ter' = mk__te env _ter in
    debug _tel';
    debug _ter';
    mk_forall_equal thm id _tel' _ter' _ty'
  | CImplL(pl,pr,q) ->
    let pl' = mk__te env pl in
    let pr' = mk__te env pr in
    let q' = mk__te env q in
    mk_impl_equal thm (mk_refl q') pl' pr' q' q'
  | CImplR(p,ql,qr) ->
    let p' = mk__te env p in
    let ql' = mk__te env ql in
    let qr' = mk__te env qr in
    mk_impl_equal (mk_refl p') thm p' p' ql' qr'
  | CAbsTy(var) -> thm
  | CForallP var -> thm

let mk_rewrite_step env is_left (rw,ctxs) =
  let env' = List.fold_right (fun x y -> env_of_ctx y x) ctxs env in
  let thm = match rw with
    | Delta(name,_tys) ->
      let thm = mk_delta env' name _tys in
      (if is_left then thm else mk_sym thm)
    | Beta(_te) ->
      let thm = mk_beta env' _te in
      (if is_left then thm else mk_sym thm)
  in
  debug thm;
  let rec fold_ctx env thm = function
    | [] -> thm
    | ctx::ctxs ->
      let env' = env_of_ctx env ctx in
      let thm' = fold_ctx env' thm ctxs in
      debug thm';
      mk_ctx env' thm' ctx
  in
  let ctxs = List.rev ctxs in

  fold_ctx env thm ctxs


let mk_rewrite_seq env side def rws =
  match rws with
  | [] -> def
  | [rw] -> mk_rewrite_step env side rw
  | rw::rws ->
    let rw = mk_rewrite_step env side rw in
    List.fold_left (fun thm rw ->
        mk_trans thm (mk_rewrite_step env side rw)) rw rws

let mk_trace env left right trace =
  let thml = mk_rewrite_seq env true left trace.left in
  let thmr = mk_rewrite_seq env false right trace.right in
  let thmr' = mk_sym thmr in
  mk_trans thml thmr'

let rec mk_proof ctx =
  let open Basic in
  function
  | Assume(j,var) -> mk_assume (mk_te ctx j.thm)
  | Lemma(cst,j) ->
    begin
      try
        thm_of_lemma (mk_qid cst)
      with _ ->
      match Env.get_type dloc (name_of cst) with
      | OK te -> mk_axiom (mk_hyp []) (mk_te ctx (CTerm.compile_term ctx te))
      | Err err -> Errors.fail_signature_error err
    end
  | ForallE(j,proof, u) ->
    begin
      match (judgment_of proof).thm with
      | Te(Forall(var,_ty,_te)) ->
        let f' = mk__te ctx (Abs(var,_ty,_te)) in
        let u' = mk__te ctx u in
        let _ty' = mk__ty _ty in
        let proof' = mk_proof ctx proof in
        mk_rule_elim_forall proof' f' _ty' u'
      | _ -> assert false
    end
  | ForallI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_ty) -> if x = var then true else false) j'.te in
    let ctx' = add_te_var ctx var _ty in
    let proof' = mk_proof ctx' proof in
    let _ty' = mk__ty _ty in
    let thm' = mk_te ctx' j'.thm in
    mk_rule_intro_forall (mk_id var) _ty' thm' proof'
  | ImplE(j,prfpq,prfp) ->
    let p = (judgment_of prfp).thm in
    let q = j.thm in
    let p' = mk_te ctx p in
    let q' = mk_te ctx q in
    let prfp' = mk_proof ctx prfp in
    let prfpq' = mk_proof ctx prfpq in
    mk_rule_elim_impl prfp' prfpq' p' q'
  | ImplI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,p = TeSet.choose (TeSet.filter (fun (x,_ty) -> if x = var then true else false) j'.hyp) in
    let q = j'.thm in
    let ctx' = add_prf_ctx ctx var (Decompile.decompile__term ctx.dk p) p in
    let p' = mk__te ctx p in
    let q' = mk_te ctx q in
    let proof' = mk_proof ctx' proof in
    mk_rule_intro_impl proof' p' q'
  | ForallPE(_,proof,_ty) -> (* WRONG: alpha *)
    begin
      match (judgment_of proof).thm with
      | ForallP(var,_) ->
        let subst = [(mk_id var, mk__ty _ty)] in
        let proof' = mk_proof ctx proof in
        mk_subst proof' subst []
      | _ -> assert false
    end
  | ForallPI(_,proof,var) ->
    let ctx' = add_ty_var ctx var in
    mk_proof ctx' proof
  | Conv(j,proof,trace) ->
    Format.eprintf "to prove: %a@." Pp.print_term (Decompile.decompile_term ctx.dk j.thm);
    Format.eprintf "from: %a@." Pp.print_term
      (Decompile.decompile_term ctx.dk (judgment_of proof).thm);
    Format.eprintf "%a@." print_trace trace;
    let right = j.thm in
    let left = (judgment_of proof).thm in
    let left' = mk_te ctx left in
    let right' = mk_te ctx right in
    mk_trace ctx (mk_refl left') (mk_refl right') trace

let print_item oc = function
  | Parameter(cst,ty) -> ()
  | Definition(cst,ty,te) ->
    let te' = mk_te empty_env te in
    mk_const (mk_qid cst) te'
  | Axiom(cst,te) ->
    let te' = mk_te empty_env te in
    let hyp = mk_hyp [] in
    mk_thm (mk_qid cst)  te' hyp (mk_axiom hyp te')
  | Theorem(cst,te,proof) ->
    Format.eprintf "Translation of %a@." Basic.pp_name (name_of cst);
    let te' = mk_te empty_env te in
    let hyp' = mk_hyp [] in
    let proof' = mk_proof empty_env proof in
    mk_thm (mk_qid cst) te' hyp' proof'
  | TyOpDef(tyop,arity) -> ()

let print_ast oc file ast =
  set_oc oc;
  version ();
  List.iter (print_item oc) ast.items;
