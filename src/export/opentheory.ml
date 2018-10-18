open Ast
open Compile
open Openstt
open Environ

(* The memoization of Openstt is not efficient and can be highly increased. For that, the memoization of openstt should be turned off and the memoization should be done in this module. One may also want to handle alpha-renaming *)

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

(* FIXME: buggy don't know why
let memoization_ty = Hashtbl.create 101

let mk__ty =
  let counter = ref (-1) in
  fun _ty ->
    if Hashtbl.mem memoization_ty _ty then
      mk_ref (Hashtbl.find memoization_ty _ty)
    else
      begin
        incr counter;
        Hashtbl.add memoization_ty _ty !counter;
        let ty' = mk__ty _ty in
        Format.eprintf "%a@." Web.print__ty _ty;
        mk_def ty' !counter
      end
*)
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
    let _ty = Env.infer ~ctx:ctx.dk cst' in
    let _ty' = CType.compile_wrapped__type ctx (Env.unsafe_reduction ~red:ST.Strategy.beta_only _ty) in
    term_of_const (const_of_name (mk_qid cst)) (mk__ty _ty')

let rec mk_te ctx = function
  | ForallP(var,te) ->
    let ctx' = add_ty_var ctx var in
    mk_te ctx' te
  | Te(_te) -> mk__te ctx _te

(* FIXME: buggy don't know why
let memoization_te = Hashtbl.create 101

let mk__te =
  let counter = ref (-1) in
  fun ctx _te ->
    if Hashtbl.mem memoization_te _te then
      mk_ref (Hashtbl.find memoization_te _te)
    else
      begin
        incr counter;
        Hashtbl.add memoization_te _te !counter;
        let te' = mk__te ctx _te in
        Format.eprintf "%a@." Web.print__te _te;
        mk_def te' !counter
      end
*)
let thm_of_const cst =
  try
    thm_of_const_name (mk_qid cst)
  with Failure _ ->
    let name = Environ.name_of cst in
    let term = Term.mk_Const Basic.dloc name in
    let te = Env.unsafe_reduction ~red:(ST.Strategy.delta name) (term) in
    let te' = CTerm.compile_term Environ.empty_env te in
    let te' = mk_te empty_env te' in
    let ty = Env.infer term in
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
    let ty = Env.get_type dloc cst in
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
  let ty = Env.infer ~ctx:[] term in
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
      try thm_of_lemma (mk_qid cst)
      with _ ->
        let te = Env.get_type dloc (name_of cst) in
        mk_axiom (mk_hyp []) (mk_te empty_env (CTerm.compile_wrapped_term empty_env te))
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
  | ForallPE(_,proof,_ty) ->
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

let content = ref ""

let pretty_print_item item = "Printing for OpenTheory is not supported right now." (*
  let print_item fmt = function
    | Parameter(name,ty) ->
      let ty' = mk_ty ty in
      let name' = mk_qid name in
      let lhs = term_of_const (const_of_name name') ty' in
      let eq = mk_equal_term lhs lhs ty' in
      mk_thm name' eq (mk_hyp []) (mk_refl lhs)
    | Definition(cst,ty,te) ->
      let cst' = mk_qid cst in
      let te' = mk_te Environ.empty_env te in
      let ty' = mk_ty ty in
      let eq = mk_equal_term (term_of_const (const_of_name cst') ty') te' ty' in
      let thm = thm_of_const cst in
      mk_thm cst' eq (mk_hyp []) thm
  | Theorem(cst,te,_)
  | Axiom(cst,te) ->
    let te' = mk_te empty_env te in
    let hyp = mk_hyp [] in
    mk_thm (mk_qid cst)  te' hyp (mk_axiom hyp te')
  | TyOpDef(tyop,arity) ->
    let tyop' = mk_qid tyop in
    let tyop' = mk_tyOp tyop' in
    let ty' = ty_of_tyOp tyop' [] in
    let name' = mk_qid ("","foo") in
    let lhs = term_of_const (const_of_name  name') ty' in
    let eq = mk_equal_term lhs lhs ty' in
    mk_thm name' eq (mk_hyp []) (mk_refl lhs)
  in
  (* let fmt = Format.formatter_of_out_channel @@ open_out "/tmp/test.art" in *)
  let str_fmt = Format.str_formatter in
  set_oc str_fmt;
  let length = Buffer.length Format.stdbuf in
  version ();
  print_item str_fmt item;
  clean ();
  let length' = Buffer.length Format.stdbuf in
  content := Buffer.sub Format.stdbuf length (length'-length);
  Buffer.truncate Format.stdbuf length;
  !content
*)

let print_item oc =
  function
  | Parameter(cst,ty) -> ()
  | Definition(cst,ty,te) ->
    (*    let te' = mk_te empty_env te in *)
    let cst' = mk_qid cst in
    let te' = mk_te Environ.empty_env te in
    let ty' = mk_ty ty in
    let eq = mk_equal_term (term_of_const (const_of_name cst') ty') te' ty' in
    let thm = thm_of_const cst in
    mk_thm cst' eq (mk_hyp []) thm
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

let print_ast oc ast =
  Buffer.clear Format.stdbuf;
  reset ();
  let oc_tmp = Format.str_formatter in
  set_oc oc_tmp;
  version ();
  List.iter (fun item -> print_item oc_tmp item) ast.items;
  clean ();
  content := Buffer.contents Format.stdbuf;
  Format.fprintf oc "%s" !content
(*
let print_meta_ast fmt meta_ast =
  let fmt_tmp = Format.str_formatter in
  set_oc fmt_tmp;
  version ();
  let print_ast ast =
    List.iter (fun item -> print_item fmt_tmp item) ast.items;
  in
  List.iter print_ast meta_ast;
  clean ();
  content := Buffer.contents Format.stdbuf;
  Format.fprintf fmt "%s" !content *)
(*
let print_bdd ast =
  Mongodb.insert_openTheory ast.md !content
*)
