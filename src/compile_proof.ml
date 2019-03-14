open Basic
open Ast
open Sttfadk
open Environ

module CType = Compile_type
module CTerm = Compile_term

let make_judgment env hyp thm = {ty= env.ty; te= env.te; hyp; thm}

let extract_te te = match te with Te _te -> _te | _ -> assert false

let rec compile_proof env proof =
  match proof with
  | Term.DB (_, _, n) ->
      let var = get_dk_var env n in
      let te' = List.assoc var env.prf in
      let j = make_judgment env (TeSet.of_list env.prf) (Te te') in
      (j, Assume(j,var))
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
    let id = gen_fresh env id in
    let jp, proof = compile_proof (add_ty_var_dk env id) _te in
    let j = make_judgment env jp.hyp (ForallP (soi id, jp.thm)) in
    (j, ForallPI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _ty), _te)
    when is_sttfa_const sttfa_etap cst || is_sttfa_const sttfa_eta cst ->
      let _ty' = CType.compile_wrapped__type env _ty in
      let id = gen_fresh env id in
      let jp, proof = compile_proof (add_te_var_dk env id _ty') _te in
      let j =
        make_judgment env jp.hyp
          (Te (Forall (soi id, _ty', extract_te jp.thm)))
      in
      (j, ForallI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _te), prf)
    when is_sttfa_const sttfa_eps cst ->
    let remove_hyp _ = TeSet.filter (fun (id',_) ->
        if string_of_ident id=id' then false else true) in
    let _te' = CTerm.compile_wrapped__term env _te in
    let jp, proof = compile_proof (add_prf_ctx env (string_of_ident id) _te _te') prf in
    let j =
      make_judgment env (remove_hyp _te' jp.hyp)
        (Te (Impl (_te', extract_te jp.thm)))
    in
    (j, ImplI (j, proof, string_of_ident id))
  | Term.Const (lc, name) ->
  let te = Env.get_type lc name in
  let te' = CTerm.compile_wrapped_term empty_env te in
  let j = make_judgment env (TeSet.of_list env.prf) te' in
    (j, Lemma (of_name name, j))
  | Term.App (f, a, args) ->
    let j,f' = compile_proof env f in
    List.fold_left (fun (j,f') a ->compile_arg env j f' a) (j,f') (a::args)
  | _ -> assert false

and compile_arg env j f' a =
  let te = Sttfatyping.subst env f' a in
  let j' = {j with thm = te} in
  let j,f' = get_product env j f' in
  match j.thm with
  | ForallP _ ->
    let a' = CType.compile__type env a in
    (j', ForallPE(j', f',a'))
  | Te Forall _ ->
    let a' = CTerm.compile__term env a in
    let proof = ForallE(j', f',a') in
    let rws,after = Sttfatyping.Tracer.annotate_beta env j'.thm in
    let trace = {left=rws; right = []} in
    let j' = {j with thm = after} in
    (j', Conv(j', proof, trace))
  | Te Impl(p',q') ->
    let ja,a' = compile_proof env a in
    let _te = match ja.thm with Te _te -> _te | _ -> assert false in
    let inferred = Impl(p', q') in
    let expected = Impl(_te,q') in
    if Sttfatyping._eq env inferred expected then
        (j', ImplE(j', f',a'))
    else
      begin
        let trace = Sttfatyping.Tracer._annotate env inferred expected in
        let f' = Conv({j with thm = Te expected}, f', trace) in
        (j', ImplE(j',f',a'))
      end
  | Te _ -> assert false

and get_product env j f' =
  match j.thm with
  | ForallP _ | Te Forall _ | Te Impl _ -> (j,f')
  | Te tyfl ->
    let _, ctx,redex = Sttfatyping.Tracer.get_app_redex true [] tyfl in
    let tyfr = Sttfatyping.Tracer._reduce env ctx redex tyfl in
    let trace = Sttfatyping.Tracer._annotate env tyfl tyfr in
    let j' = {j with thm = Te tyfr} in
    let proof' = Conv(j',f',trace) in
    get_product env j' proof'
