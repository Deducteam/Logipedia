open Ast
open Compile
open Openstt
open Environ

module ST = Sttfatyping

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



let thm_of_const cst =
  try
    thm_of_const_name (mk_qid cst)
  with Failure _ ->
    let name = Environ.name_of cst in
    let term = Term.mk_Const Basic.dloc name in
    let te = Env.unsafe_reduction ~red:(ST.Strategy.delta name) (term) in
    let te' = CTerm.compile_term Environ.empty_env te in
    let te' = mk_te empty_env te' in
    let ty = match Env.infer term with | Basic.OK ty -> ty | _ -> assert false in
    let ty' = CType.compile_wrapped_type Environ.empty_env ty in
    let ty' = mk_ty ty' in
    let const = const_of_name (mk_qid cst) in
    let constterm = term_of_const const ty' in
    let eq = mk_equal_term constterm te' ty' in
    mk_axiom (mk_hyp []) eq


let add_prf_ctx env id _te _te' =
  { env with
    k= env.k + 1
  ; prf= (id, _te') :: env.prf
  ; dk= (Basic.dloc, Basic.mk_ident id, _te) :: env.dk }

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
    let thm = thm_of_const (md,id) in
    mk_subst thm (List.combine vars' _tys') []

let mk_beta env _te =
  let _te' = mk__te env _te in
  mk_betaConv _te'

let mk_delta ctx cst _tys =
  let open Basic in
  let thm = thm_of_const cst in
  let term =
    Term.mk_Const dloc (mk_name (mk_mident (fst cst)) (mk_ident (snd cst)))
  in
  let ty =
    match Env.infer ~ctx:[] term  with
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

let rec mk__ctx env thm ctx left right =
  match ctx,left,right with
  | [], _ , _-> thm
  | CAbsTy::ctx, AbsTy(var,_te), AbsTy(var',_te') ->
    mk__ctx env thm ctx _te _te'
  | CAbs::ctx, Abs(var,_ty,_te), Abs(var',_ty',_te') ->
    assert (var = var');
    assert (_ty = _ty');
    let env' = add_te_var env var _ty in
    let var = mk_var (mk_id var) (mk__ty _ty) in
    let thm = mk__ctx env' thm ctx _te _te' in
    mk_absThm var thm
  | CForall::ctx, Forall(var,_ty,_tel), Forall(var',_ty',_ter) ->
    assert (var = var');
    assert (_ty = _ty');
    let env' = add_te_var env var _ty in
    let _tel' = mk__te env' _tel in
    let _ter' = mk__te env' _ter in
    let thm = mk__ctx env' thm ctx _tel _ter in
    mk_forall_equal thm (mk_id var) _tel' _ter' (mk__ty _ty)
  | CAppL::ctx, App(_tel,_ter), App(_tel',_ter') ->
    let thm = mk__ctx env thm ctx _tel _tel' in
    mk_appThm thm (mk_refl (mk__te env _ter))
  | CAppR::ctx, App(_tel,_ter), App(_tel',_ter') ->
    let thm = mk__ctx env thm ctx _ter _ter' in
     mk_appThm (mk_refl (mk__te env _tel)) thm
  | CImplL::ctx, Impl(_tel1, _ter1), Impl(_tel2, _ter2) ->
    let _tel1' = mk__te env _tel1 in
    let _ter1' = mk__te env _ter1 in
    let _tel2' = mk__te env _tel2 in
    let _ter2' = mk__te env _ter2 in
    let thm = mk__ctx env thm ctx _tel1 _tel2 in
    mk_impl_equal thm (mk_refl _ter1') _tel1' _ter1' _tel2' _ter2'
  | CImplR::ctx, Impl(_tel1, _ter1), Impl(_tel2, _ter2) ->
    let _tel1' = mk__te env _tel1 in
    let _ter1' = mk__te env _ter1 in
    let _tel2' = mk__te env _tel2 in
    let _ter2' = mk__te env _ter2 in
    let thm = mk__ctx env thm ctx _ter1 _ter2 in
    mk_impl_equal (mk_refl _tel1') thm _tel1' _ter1' _tel2' _ter2'
  | _ -> assert false

let rec mk_ctx env thm ctx left right =
  match ctx, left,right with
  | CForallP::ctx, ForallP(var,_te) , ForallP(_,_te') ->
    let env' = add_ty_var env var in
    let thm = mk_ctx env' thm ctx _te _te' in
    thm
  | _, Te _te, Te _te' -> mk__ctx env thm ctx  _te _te'
  | _, _,_ -> assert false

let mk_rewrite_step env term (redex,ctx) =
  let env' = Sttfatyping.Tracer.env_of_redex env ctx term in
  let term' = Sttfatyping.Tracer.reduce env' ctx redex term in
  let thm = match redex with
    | Delta(name,_tys) -> mk_delta env' name _tys
    | Beta(_te) -> mk_beta env' _te
  in
  let thm =  mk_ctx env thm ctx term term' in
  term',thm

let mk_rewrite_seq env term rws =
  match rws with
  | [] -> term, mk_refl (mk_te env term)
  | [rw] ->  mk_rewrite_step env term rw
  | rw::rws ->
    let term',rw = mk_rewrite_step env term rw in
    List.fold_left (fun (term,thm) rw ->
        let term', thm' = (mk_rewrite_step env term rw) in
        term', mk_trans thm thm') (term',rw) rws

let mk_trace env left right trace =
  let _,thml = mk_rewrite_seq env left trace.left in
  let _,thmr = mk_rewrite_seq env right trace.right in
  let thmr' = mk_sym thmr in
  mk_trans thml thmr'

let rec mk_proof env =
  let open Basic in
  function
  | Assume(j,var) -> mk_assume (mk_te env j.thm)
  | Lemma(cst,j) ->
    begin
      try
        thm_of_lemma (mk_qid cst)
      with _ ->
      match Env.get_type dloc (name_of cst) with
      | OK te ->
        mk_axiom (mk_hyp []) (mk_te empty_env (CTerm.compile_wrapped_term empty_env te))
      | Err err -> Errors.fail_signature_error err
    end
  | ForallE(j,proof, u) ->
    begin
      match (judgment_of proof).thm with
      | Te(Forall(var,_ty,_te)) ->
        let f' = mk__te env (Abs(var,_ty,_te)) in
        let u' = mk__te env u in
        let _ty' = mk__ty _ty in
        let proof' = mk_proof env proof in
        mk_rule_elim_forall proof' f' _ty' u'
      | _ -> assert false
    end
  | ForallI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_ty) -> if x = var then true else false) j'.te in
    let env' = add_te_var env var _ty in
    let proof' = mk_proof env' proof in
    let _ty' = mk__ty _ty in
    let thm' = mk_te env' j'.thm in
    mk_rule_intro_forall (mk_id var) _ty' thm' proof'
  | ImplE(j,prfpq,prfp) ->
    let p = (judgment_of prfp).thm in
    let q = j.thm in
    let p' = mk_te env p in
    let q' = mk_te env q in
    let prfp' = mk_proof env prfp in
    let prfpq' = mk_proof env prfpq in
    mk_rule_elim_impl prfp' prfpq' p' q'
  | ImplI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,p = TeSet.choose (TeSet.filter (fun (x,_ty) -> if x = var then true else false) j'.hyp) in
    let q = j'.thm in
    let env' = add_prf_ctx env var (Decompile.decompile__term env.dk p) p in
    let p' = mk__te env p in
    let q' = mk_te env q in
    let proof' = mk_proof env' proof in
    mk_rule_intro_impl proof' p' q'
  | ForallPE(_,proof,_ty) -> (* MIGHT BE WRONG ? *)
    begin
      match (judgment_of proof).thm with
      | ForallP(var,_) ->
        let subst = [(mk_id var, mk__ty _ty)] in
        let proof' = mk_proof env proof in
        mk_subst proof' subst []
      | _ -> assert false
    end
  | ForallPI(_,proof,var) ->
    let env' = add_ty_var env var in
    mk_proof env' proof
  | Conv(j,proof,trace) ->
    let right = j.thm in
    let left = (judgment_of proof).thm in
    let proof = mk_proof env proof in
    let mp = mk_eqMp proof (mk_trace env left right trace) in
    mp

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
    let te' = mk_te empty_env te in
    let hyp' = mk_hyp [] in
    let proof' = mk_proof empty_env proof in
    mk_thm (mk_qid cst) te' hyp' proof'
  | TyOpDef(tyop,arity) -> ()

let print_ast oc file ast =
  set_oc oc;
  version ();
  List.iter (print_item oc) ast.items

let parameters = Mongo.create_local_default "logipedia" "parameters"
let definitions = Mongo.create_local_default "logipedia" "definitions"
let theoremes = Mongo.create_local_default "logipedia" "theoremes"
let axiomes = Mongo.create_local_default "logipedia" "axiomes"

let empty_doc = Bson.empty

let insert_parameter (md,id) ty =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "type" (Bson.create_string (ty)) key_doc_2 in
  let key_doc_4 = Bson.add_element "langID" (Bson.create_string ("6")) key_doc_3 in
  Mongo.insert parameters [key_doc_4]

let insert_definition (md,id) ty te =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "type" (Bson.create_string (ty)) key_doc_2 in
  let key_doc_4 = Bson.add_element "statement" (Bson.create_string (te)) key_doc_3 in
  let key_doc_5 = Bson.add_element "langID" (Bson.create_string ("6")) key_doc_4 in
  Mongo.insert definitions [key_doc_5]

let insert_theorem (md,id) te proof =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "statement" (Bson.create_string (te)) key_doc_2 in
  let key_doc_4 = Bson.add_element "proof" (Bson.create_string (proof)) key_doc_3 in
  let key_doc_5 = Bson.add_element "langID" (Bson.create_string ("6")) key_doc_4 in
  Mongo.insert theoremes [key_doc_5]

let insert_axiom (md,id) te =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "statement" (Bson.create_string (te)) key_doc_2 in
  let key_doc_4 = Bson.add_element "langID" (Bson.create_string ("6")) key_doc_3 in
  Mongo.insert axiomes [key_doc_4]

let to_string fmt = Format.asprintf "%a" fmt
