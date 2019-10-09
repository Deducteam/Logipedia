open Environ

module CType = Compile_type
module CT = Compile_term

module A = Ast
module B = Kernel.Basic
module Env = Api.Env
module Sfa = Sttfadk
module T = Kernel.Term

let make_judgment env hyp thm = {A.ty= env.ty; te= env.te; hyp; thm}

let extract_te te = match te with A.Te _te -> _te | _ -> assert false

let rec compile_proof dkenv env proof =
  match proof with
  | T.DB (_, _, n) ->
      let var = get_dk_var env n in
      let te' = List.assoc var env.prf in
      let j = make_judgment env (A.TeSet.of_list env.prf) (Te te') in
      (j, A.Assume(j,var))
  | T.Lam (_, id, Some cst, _te) when Sfa.is_sttfa_const Sfa.sttfa_type cst ->
    let id = gen_fresh env id in
    let jp, proof = compile_proof dkenv (add_ty_var_dk env id) _te in
    let j = make_judgment env jp.hyp (ForallP (soi id, jp.thm)) in
    (j, ForallPI (j, proof, soi id))
  | T.Lam (_, id, Some (T.App (cst, _, _) as _ty), _te)
    when Sfa.is_sttfa_const Sfa.sttfa_etap cst ||
         Sfa.is_sttfa_const Sfa.sttfa_eta cst ->
      let _ty' = CType.compile_wrapped__type dkenv env _ty in
      let id = gen_fresh env id in
      let jp, proof = compile_proof dkenv (add_te_var_dk env id _ty') _te in
      let j =
        make_judgment env jp.hyp
          (Te (Forall (soi id, _ty', extract_te jp.thm)))
      in
      (j, ForallI (j, proof, soi id))
  | T.Lam (_, id, Some (T.App (cst, _, _) as _te), prf)
    when Sfa.is_sttfa_const Sfa.sttfa_eps cst ->
    let remove_hyp _ = A.TeSet.filter (fun (id',_) ->
        if B.string_of_ident id=id' then false else true) in
    let _te' = CT.compile_wrapped__term dkenv env _te in
    let jp, proof = compile_proof dkenv
        (add_prf_ctx env (B.string_of_ident id) _te _te') prf
    in
    let j =
      make_judgment env (remove_hyp _te' jp.hyp)
        (Te (Impl (_te', extract_te jp.thm)))
    in
    (j, ImplI (j, proof, B.string_of_ident id))
  | T.Const (lc, name) ->
  let te = Env.get_type dkenv lc name in
  let te' = CT.compile_wrapped_term empty_env dkenv te in
  let j = make_judgment env (A.TeSet.of_list env.prf) te' in
    (j, Lemma (of_name name, j))
  | T.App (f, a, args) ->
    let j,f' = compile_proof dkenv env f in
    List.fold_left (fun (j,f') a ->compile_arg dkenv env j f' a) (j,f') (a::args)
  | _ -> assert false

and compile_arg (dkenv:Env.t) env j f' a =
  let (module Tracer) = Sttfatyping.mk_tracer dkenv in
  let te = Sttfatyping.subst dkenv env f' a in
  let j' = {j with thm = te} in
  let j,f' = get_product dkenv env j f' in
  match j.A.thm with
  | ForallP _ ->
    let a' = CType.compile__type dkenv env a in
    (j', ForallPE(j', f',a'))
  | Te Forall _ ->
    let a' = CT.compile__term dkenv env a in
    let proof = A.ForallE(j', f',a') in
    let rws,after = Tracer.annotate_beta env j'.thm in
    let trace = {A.left=rws; right = []} in
    let j' = {j with thm = after} in
    (j', Conv(j', proof, trace))
  | Te Impl(p',q') ->
    let ja,a' = compile_proof dkenv env a in
    let _te = match ja.thm with Te _te -> _te | _ -> assert false in
    let inferred = A.Impl(p', q') in
    let expected = A.Impl(_te,q') in
    if Sttfatyping._eq env inferred expected then
        (j', ImplE(j', f',a'))
    else
      begin
        let trace = Tracer._annotate env inferred expected in
        let f' = A.Conv({j with thm = Te expected}, f', trace) in
        (j', ImplE(j',f',a'))
      end
  | Te _ -> assert false

and get_product (dkenv:Env.t) env j f' =
  let (module Tracer) = Sttfatyping.mk_tracer dkenv in
  match j.thm with
  | ForallP _ | Te Forall _ | Te Impl _ -> (j,f')
  | Te tyfl ->
    let _, ctx,redex = Tracer.get_app_redex true [] tyfl in
    let tyfr = Tracer._reduce env ctx redex tyfl in
    let trace = Tracer._annotate env tyfl tyfr in
    let j' = {j with thm = Te tyfr} in
    let proof' = A.Conv(j',f',trace) in
    get_product dkenv env j' proof'
