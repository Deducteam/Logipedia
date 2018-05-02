open Ast
open Compile
open Openstt

type proof_ctx = (string * _te) list

type env = Compile.env

let name_of cst = Basic.mk_name (Basic.mk_mident (fst cst)) (Basic.mk_ident (snd cst))

let add_ty_var env var =
  let open Basic in
  let open Sttforall in
  { env with
    k= env.k + 1
  ; ty= var :: env.ty
  ; dk=
      (dloc, mk_ident var, Term.mk_Const dloc (mk_name sttfa_module sttfa_type))
      :: env.dk }

let add_te_var env var ty' =
  let open Basic in
  let ty = Decompile.decompile__type env.dk ty' in
  let ty = Decompile.to__type ty in
  { env with
    k = env.k + 1;
    te= (var, ty') :: env.te; dk= (dloc, mk_ident var, ty) :: env.dk
  }

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
      let _ty' = Compile.compile_wrapped__type ctx (Env.unsafe_reduction ~red:beta_only _ty) in
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
      | OK te -> mk_axiom (mk_hyp []) (mk_te ctx (Compile.compile_term ctx te))
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
  | ForallI(j,proof,var) -> failwith "todo"
  | ImplE(j,prfpq,prfp) ->
    let p = (judgment_of prfp).thm in
    let q = j.thm in
    let p' = mk_te ctx p in
    let q' = mk_te ctx q in
    let prfp' = mk_proof ctx prfp in
    let prfpq' = mk_proof ctx prfpq in
    mk_rule_elim_impl prfp' prfpq' p' q'
  | ImplI(j,proof,var) -> failwith "todo"
  | ForallPE(_,proof,_ty) ->
    begin
      match (judgment_of proof).thm with
      | ForallP(var,_) ->
        let subst = [(mk_id var, mk__ty _ty)] in
        let proof' = mk_proof ctx proof in
        mk_subst proof' subst []
      | _ -> assert false
    end
  | ForallPI(_,proof,var) -> failwith "todo"
  | Conv(_,proof,_) -> failwith "todo"

let rec print_item oc = function
  | Parameter(cst,ty) -> ()
  | Definition(cst,ty,te) ->
    let te' = mk_te empty_env te in
    mk_const (mk_qid cst) te'
  | Axiom(cst,te) ->
    let te' = mk_te empty_env te in
    let hyp = mk_hyp [] in
    mk_thm (mk_qid cst)  te' hyp (mk_axiom hyp te')
  | Theorem(name,te,proof) -> print_item oc (Axiom(name,te))
  | TyOpDef(tyop,arity) -> ()

let print_ast oc file ast =
  set_oc oc;
  List.iter (print_item oc) ast.items;
