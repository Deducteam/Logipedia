open Core.Extras
open Ast
open Kernel.Basic
open Environ

module Env = Api.Env
module Dpp = Api.Pp.Default

module Reduction = Kernel.Reduction
module Rule = Kernel.Rule
module Subst = Kernel.Subst

(* TODO: Enhance error messages *)

module CType = Compile_type
module CTerm = Compile_term

let infer denv env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  CType.compile_type denv env (Env.infer denv ~ctx:env.dk tedk)

let _infer denv env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  let ty = Env.infer denv ~ctx:env.dk tedk in
  try CType.compile__type denv env ty
  with e ->
    Env.fail_env_error denv Kernel.Basic.dloc e
    (* Derr.fail_exit 1 "Inference fail" None (Some dloc)
     *   "Inference failed because type is polymorphic" ; *)

let _eq env left right =
  let leftdk = Decompile.decompile__term env.dk left in
  let rightdk = Decompile.decompile__term env.dk right in
  Term.term_eq leftdk rightdk

let eq env left right =
  let leftdk = Decompile.decompile_term env.dk left in
  let rightdk = Decompile.decompile_term env.dk right in
  Term.term_eq leftdk rightdk

let are_convertible denv env left right =
  let leftdk = Decompile.decompile__term env.dk left in
  let rightdk = Decompile.decompile__term env.dk right in
  Env.are_convertible denv ~ctx:env.dk leftdk rightdk

let print__te env fmt _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  Format.fprintf fmt "%a@." Dpp.print_term _tedk

let print_te env fmt te =
  let tedk = Decompile.decompile_term env.dk te in
  Format.fprintf fmt "%a@." Dpp.print_term tedk

module ComputeStrategy =
struct
  open Reduction

  let one_whnf = { default_cfg with nb_steps = Some 1; target = Whnf }

  let beta_snf = { default_cfg with select = Some (fun _ -> false) }
  let beta_steps n = { beta_snf with nb_steps = Some n}

  let delta (cst:Basic.name) =
    let open Rule in
    let filter = function Delta cst' -> name_eq cst' cst | _ -> false in
    { default_cfg with
      nb_steps = Some 1; beta = false; target = Snf; select = Some filter }

  let beta_only = beta_snf

  let beta_only_n (n:int) = beta_steps n

  let beta_one = { beta_only with nb_steps = Some 1; target = Whnf }

  let beta_delta_only =
    let open Rule in
    let filter = function Delta _ -> true | _ -> false in
    { beta_only with select = Some filter}

  let delta_only cst  = delta cst

end

let _is_beta_normal denv env _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  let _tedk' = Env.unsafe_reduction denv ~red:ComputeStrategy.beta_snf _tedk in
  Term.term_eq _tedk _tedk'

let is_beta_normal denv env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' = Env.unsafe_reduction denv ~red:ComputeStrategy.beta_snf tedk in
  Term.term_eq tedk tedk'

let _beta_reduce denv env _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  let _tedk' = Env.unsafe_reduction denv ~red:(ComputeStrategy.beta_steps 1) _tedk
  in
  CTerm.compile__term denv env _tedk'

let beta_reduce denv env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' = Env.unsafe_reduction denv ~red:(ComputeStrategy.beta_steps 1) tedk in
  CTerm.compile_term denv env tedk'

let beta_normal denv env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' = Env.unsafe_reduction denv ~red:(ComputeStrategy.beta_snf) tedk in
  CTerm.compile_term denv env tedk'

let subst denv env f a =
  let thm = (judgment_of f).thm in
  let te = Decompile.to_eps (Decompile.decompile_term env.dk thm) in
  let te = Env.unsafe_reduction denv ~red:(ComputeStrategy.one_whnf) te in
  let _,b = match te with
    | Term.Pi(_,_,tya,tyb) -> tya,tyb
    | _ -> assert false
  in
  let b' = Subst.subst b a in
  let b' =
    match b' with
    | Term.App(_, a, []) -> a
    | _ -> assert false
  in
  let b' = Env.unsafe_reduction denv ~red:(ComputeStrategy.beta_one) b' in
  CTerm.compile_term denv env b'

(** This module aims to implement functions that trace reduction steps
    checking if two terms are convertible. *)
module Tracer =
struct

  let fast = ref false

  let is_defined denv cst =
    let name = name_of cst in
    not (DkTools.is_static (Env.get_signature denv) dloc name)


  let rec is_redexable denv _te =
    match _te with
    | Cst(cst, _) -> is_defined denv cst
    | App(Abs _,_) -> true
    | App(f,_) -> is_redexable denv f
    | _ ->  false

  let rec get_app_redex denv side ctx tyf' =
    match tyf' with
    | App(Abs _, _te) -> side, ctx, Beta(tyf')
    | App (f, _)      -> get_app_redex denv side (CAppL::ctx) f
    | Cst(cst, _tys)  ->
      assert (is_defined denv cst);
      side, ctx, Delta (cst, _tys)
    | _ -> assert false

  exception Equal
  exception Maybe

  let rec _get_beta_redex env ctx term =
    match term with
    | TeVar _ -> raise Not_found
    | Cst(_,_tys) -> raise Not_found
    | Abs(var,_ty,_te) ->
      let env' = add_te_var env var _ty in
      _get_beta_redex env' (CAbs::ctx) _te
    | AbsTy(var,_te) ->
      let env' = add_ty_var env var in
      _get_beta_redex env' (CAbsTy::ctx) _te
    | Forall(var,_ty,_te) ->
      let env' = add_te_var env var _ty in
      _get_beta_redex env' (CForall::ctx) _te
    | Impl(_tel,_ter) ->
      begin
        try
          _get_beta_redex env (CImplL::ctx) _tel
        with Not_found ->
          _get_beta_redex env (CImplR::ctx) _ter
      end
    | App(Abs _,_) -> (ctx, Beta(term))
    | App(_tel,_ter) ->
      begin
        try
          _get_beta_redex env (CAppL::ctx) _tel
        with Not_found ->
          _get_beta_redex env (CAppR::ctx) _ter
      end

  let rec get_beta_redex env ctx term =
    match term with
    | Te term -> _get_beta_redex env ctx term
    | ForallP(var,term)  ->
      let env' = add_ty_var env var in
      get_beta_redex env' (CForallP::ctx) term


  let get_beta_redex env ctx term =
    let ctx,redex = get_beta_redex env ctx term in
    List.rev ctx, redex

  let _get_beta_redex env ctx term =
    let ctx,redex = _get_beta_redex env ctx term in
    List.rev ctx, redex

  let rec _get_redex denv (envl,envr) ctx (left,right) =
    (* Format.eprintf "left:%a@." (print__te envl) left;
    Format.eprintf "right:%a@." (print__te envr) right; *)
    match (left,right) with
    | TeVar _, TeVar _ -> raise Equal
    | Cst(cst,_tys), Cst(cst', _tys') ->
      if is_defined denv cst then
        if is_defined denv cst' then
          if cst = cst' then
            raise Maybe
          else
            (true, ctx, Delta(cst, _tys))
        else
          (true, ctx, Delta(cst,_tys))
      else if is_defined denv cst' then
        (false, ctx, Delta(cst',_tys'))
      else
        raise Equal
    | Cst (cst,_tys) , _ when is_defined denv cst ->
      (true, ctx, Delta(cst, _tys))
    | _ , Cst(cst,_tys) when is_defined denv cst ->
      (false, ctx, Delta(cst, _tys))
    | Abs (var, _ty, _tel), Abs (var', _ty', _ter) ->
      let envl' = add_te_var envl var _ty in
      let envr' = add_te_var envr var' _ty' in
      _get_redex denv (envl',envr') (CAbs::ctx) (_tel,_ter)
    | Forall (var, _ty, _tel), Forall (var', _ty', _ter) ->
      let envl' = add_te_var envl var _ty in
      let envr' = add_te_var envr var' _ty' in
      _get_redex denv (envl',envr') (CForall::ctx) (_tel,_ter)
    | Impl (_tel, _ter), Impl (_tel', _ter') ->
      begin
        try _get_redex denv (envl,envr) (CImplL::ctx) (_tel,_tel')
        with Equal | Maybe ->  _get_redex denv (envl,envr) (CImplR::ctx) (_ter,_ter')
      end
    | App(Abs _, _), _ ->
      (true, ctx, Beta(left))
    | _, App(Abs _, _) ->
      (false, ctx, Beta(right))
    | App(_tel, _ter) , App(_tel', _ter') ->
      let leftdk = Decompile.decompile__term envl.dk left in
      let rightdk = Decompile.decompile__term envr.dk right in
      if Term.term_eq leftdk rightdk then
        raise Equal
      else
      if is_redexable denv _tel then
        get_app_redex denv true ctx left
      else if is_redexable denv _tel' then
        get_app_redex denv false ctx right
      else
        begin
          try
            _get_redex denv (envl,envr) (CAppL::ctx) (_tel, _tel')
          with Equal ->
            _get_redex denv (envl,envr) (CAppR::ctx) (_ter,_ter')
        end
    | App _ , _ ->
      get_app_redex denv true ctx left
    | _, (App _) ->
      get_app_redex denv false ctx right
    | _ -> assert false

  let rec get_redex denv (envl,envr) ctx = function
    | Te left, Te right -> _get_redex denv (envl,envr) ctx (left,right)
    | ForallP(var,left), ForallP(var', right) ->
      let envl' = add_ty_var envl var in
      let envr' = add_ty_var envr var' in
      get_redex denv (envl',envr') (CForallP::ctx) (left,right)
    | _ -> assert false

  let get_redex denv env ctx lr =
    let is_left,ctx,redex = get_redex denv env ctx lr in
    is_left, List.rev ctx, redex

  let _get_redex denv env ctx lr =
    let is_left,ctx,redex = _get_redex denv env ctx lr in
    is_left, List.rev ctx, redex

  let rec _env_of_redex env ctx term =
    match ctx,term with
    | [],_ -> env
    | CAppR::ctx, App(_,_ter) -> _env_of_redex env ctx _ter
    | CAppL::ctx, App(_tel,_) -> _env_of_redex env ctx _tel
    | CImplR::ctx, Impl(_,_ter) -> _env_of_redex env ctx _ter
    | CImplL::ctx, Impl(_tel,_) -> _env_of_redex env ctx _tel
    | CForall::ctx, Forall(var,_ty,_te) ->
      let env' = add_te_var env var _ty in
      _env_of_redex env' ctx _te
    | CAbs::ctx, Abs(var,_ty,_te) ->
      let env' = add_te_var env var _ty in
      _env_of_redex env' ctx _te
    | _ -> assert false

  let rec env_of_redex env ctx term =
    match ctx, term with
    | [], _ -> env
    | CForallP::ctx, ForallP(var,term) ->
      let env' = add_ty_var env var in
      env_of_redex env' ctx term
    | _, Te _te -> _env_of_redex env ctx _te
    | _ -> assert false

  let rec _apply ctx newterm term =
    match ctx,term with
    | [], _ -> newterm
    | CAppR::ctx, App(_tel,_ter) -> App(_tel, _apply ctx newterm _ter)
    | CAppL::ctx, App(_tel,_ter) -> App(_apply ctx newterm _tel, _ter)
    | CImplR::ctx, Impl(_tel,_ter) -> Impl(_tel, _apply ctx newterm _ter)
    | CImplL::ctx, Impl(_tel,_ter) -> Impl(_apply  ctx newterm _tel, _ter)
    | CForall::ctx, Forall(var,_ty,_te) ->
      Forall(var,_ty,_apply ctx newterm _te)
    | CAbs::ctx, Abs(var,_ty,_te) ->
      Abs(var,_ty,_apply ctx newterm _te)
    | _ -> assert false

  let rec apply ctx newterm term =
    match ctx, term with
    | CForallP::ctx, ForallP(var, te) ->
      ForallP(var, apply ctx newterm te)
    | _, Te _te -> Te(_apply ctx newterm _te)
    | _ -> assert false

  let newterm denv env _ redex =
      match redex with
      | Beta(_te) ->
        let _tedk = Decompile.decompile__term env.dk _te in
        CTerm.compile__term denv env
          (Env.unsafe_reduction denv ~red:ComputeStrategy.beta_one _tedk)
      | Delta(cst,_tys) ->
        let name = name_of cst in
        let _tedk = Decompile.decompile__term env.dk (Cst(cst,_tys)) in
        (* These two steps might be buggy in the future since we use
           SNF instead of WHNF because of the coercion eps *)
        let _tedk' = Env.unsafe_reduction denv ~red:(ComputeStrategy.delta name)
            _tedk
        in
        let _tedk' = Env.unsafe_reduction denv
            ~red:(ComputeStrategy.beta_steps (List.length _tys)) _tedk'
        in
        CTerm.compile__term denv env _tedk'

  let _reduce denv env ctx redex _te =
    let newterm = newterm denv env ctx redex in
    _apply ctx newterm _te


  let reduce denv env ctx redex te =
    let newterm = newterm denv env ctx redex in
    apply ctx newterm te

  type 'a step =
    {
      is_left: bool;
      redex: redex;
      ctx: ctx list
    }

  let get_step is_left redex ctx = {is_left;redex;ctx}

  let _one_step denv env left right =
    let is_left,ctx,redex = _get_redex denv (env,env) [] (left,right) in
    if is_left then
      let env' = _env_of_redex env ctx left in
      let left' = _reduce denv env' ctx redex left in
      let step = get_step is_left redex ctx in
      step, left', right
    else
      let env' = _env_of_redex env ctx right in
      let right' = _reduce denv env' ctx redex right in
      let step = get_step is_left redex ctx in
      step, left, right'

  let one_step denv env left right =
    let is_left,ctx,redex = get_redex denv (env,env) [] (left,right) in
    if is_left then
      let env' = env_of_redex env ctx left in
      let left' = reduce denv env' ctx redex left in
      let step = get_step is_left redex ctx in
      step, left', right
    else
      let env' = env_of_redex env ctx right in
      let right' = reduce denv env' ctx redex right in
      let step = get_step is_left redex ctx in
      step, left, right'

  let empty_trace = {left= []; right = []}

  let rec _annotate_beta denv env _te =
    if _is_beta_normal denv env _te then
      [], _te
    else
      let ctx,redex = _get_beta_redex env [] _te in
      let env' = _env_of_redex env ctx _te in
      let _te' = _reduce denv env' ctx redex _te in
      let trace, _tenf = _annotate_beta denv env _te' in
      (redex, ctx)::trace, _tenf

  let rec annotate_beta denv env te =
    if is_beta_normal denv env te then
      [], te
    else
      let ctx,redex = get_beta_redex env [] te in
      let env' = env_of_redex env ctx te in
      let te' = reduce denv env' ctx redex te in
      let trace, tenf = annotate_beta denv env te' in
      (redex, ctx)::trace, tenf

  let annotate_beta denv env te =
    if !fast then
      [], te
    else
      annotate_beta denv env te

  let rec _annotate denv env left right =
    if _eq env left right then
      empty_trace
    else
      let tracel,left' = _annotate_beta denv env left in
      let tracer, right' = _annotate_beta denv env right in
      let trace_beta = {left=tracel; right =tracer} in
      if _eq env left' right' then
        trace_beta
      else
        let step, left', right' = _one_step denv env left' right' in
        let trace' = _annotate denv env left' right' in
        let trace'' =
          if step.is_left then
            {trace' with left=(step.redex, step.ctx)::trace'.left}
          else
            {trace' with right=(step.redex, step.ctx)::trace'.right}
        in
        {left = trace_beta.left@trace''.left;
         right= trace_beta.right@trace''.right}

  let _annotate denv env left right =
    if !fast then
      empty_trace
    else
      _annotate denv env left right

  let rec annotate denv env left right =
    if eq env left right then
      empty_trace
    else
      let tracel,left' = annotate_beta denv env left in
      let tracer, right' = annotate_beta denv env right in
      let trace_beta = {left=tracel; right =tracer} in
      if eq env left' right' then
        trace_beta
      else
        let step, left', right' = one_step denv env left' right' in
        let trace' = annotate denv env left' right' in
        let trace'' =
          if step.is_left then
            {trace' with left=(step.redex, step.ctx)::trace'.left}
          else
            {trace' with right=(step.redex, step.ctx)::trace'.right}
        in
        {left = trace_beta.left@trace''.left;
         right= trace_beta.right@trace''.right}

  let annotate denv env left right =
    if !fast then
      empty_trace
    else
      annotate denv env left right
end
