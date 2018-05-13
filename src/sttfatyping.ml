open Ast
open Basic
open Environ

(* TODO: Enhance error messages *)

module CType = Compile_type
module CTerm = Compile_term

let infer env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  match Env.infer ~ctx:env.dk tedk with
  | OK ty -> CType.compile_type env ty
  | Err err -> Errors.fail_env_error err

let _infer env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  match Env.infer ~ctx:env.dk tedk with
  | OK ty ->
    begin
      try
        CType.compile__type env ty
      with _ -> Errors.fail dloc "Inference failed because type is polymorphic"
    end
  | Err err -> Errors.fail_env_error err

let _eq env left right =
  let leftdk = Decompile.decompile__term env.dk left in
  let rightdk = Decompile.decompile__term env.dk right in
  Term.term_eq leftdk rightdk

let eq env left right =
  let leftdk = Decompile.decompile_term env.dk left in
  let rightdk = Decompile.decompile_term env.dk right in
  Term.term_eq leftdk rightdk

let are_convertible env left right =
  let leftdk = Decompile.decompile__term env.dk left in
  let rightdk = Decompile.decompile__term env.dk right in
  Env.are_convertible ~ctx:env.dk leftdk rightdk

let print__te env fmt _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  Format.fprintf fmt "%a@." Pp.print_term _tedk

let print_te env fmt te =
  let tedk = Decompile.decompile_term env.dk te in
  Format.fprintf fmt "%a@." Pp.print_term tedk

module Strategy =
struct


  let one_whnf : Reduction.red_cfg =
    let open Reduction in
    { nb_steps= Some 1
    ; beta= true
    ; strategy= Reduction.Whnf
    ; select= None }

  let beta_snf : Reduction.red_cfg =
    let open Reduction in
    { nb_steps= None
    ; beta= true
    ; strategy= Reduction.Snf
    ; select= Some (fun _ -> false) }

  let beta_one =
    let open Reduction in
    { nb_steps= Some 1
    ; beta= true
    ; strategy= Reduction.Whnf
    ; select= Some (fun _ -> false) }

  let beta_steps n =
    let open Reduction in
    { nb_steps= Some n
    ; beta= true
    ; strategy= Reduction.Snf
    ; select= Some (fun _ -> false) }

  let delta : Basic.name -> Reduction.red_cfg =
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
end

let _is_beta_normal env _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  let _tedk' = Env.unsafe_reduction ~red:Strategy.beta_snf _tedk in
  Term.term_eq _tedk _tedk'

let is_beta_normal env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' = Env.unsafe_reduction ~red:Strategy.beta_snf tedk in
  Term.term_eq tedk tedk'

let _beta_reduce env _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  let _tedk' = Env.unsafe_reduction ~red:(Strategy.beta_steps 1) _tedk in
  CTerm.compile__term env _tedk'

let beta_reduce env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' = Env.unsafe_reduction ~red:(Strategy.beta_steps 1) tedk in
  CTerm.compile_term env tedk'

let beta_normal env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' = Env.unsafe_reduction ~red:(Strategy.beta_snf) tedk in
  CTerm.compile_term env tedk'

let subst env f a =
  let thm = (judgment_of f).thm in
  let te = Decompile.to_eps (Decompile.decompile_term env.dk thm) in
  let te = Env.unsafe_reduction ~red:(Strategy.one_whnf) te in
  let _,b = match te with | Term.Pi(_,var,tya,tyb) -> tya,tyb | _ -> assert false in
  let b' = Subst.subst b a in
  let b' = Env.unsafe_reduction ~red:(Strategy.beta_steps 1) b' in
  CTerm.compile_wrapped_term env b'

module Tracer =
struct
  let is_defined cst =
    let name = name_of cst in
    not (Env.is_static dloc name)


  let rec is_redexable _te =
    match _te with
    | Cst(cst, _) ->is_defined cst
    | App(Abs _,_) -> true
    | App(f,_) -> is_redexable f
    | _ ->  false

  let rec get_app_redex side ctx tyf' =
    match tyf' with
    | App(Abs _, _te) -> side, ctx, Beta(tyf')
    | App (f, a)      -> get_app_redex side (CAppL::ctx) f
    | Cst(cst, _tys)  ->
      assert (is_defined cst);
      side, ctx, Delta (cst, _tys)
    | _ -> assert false

  exception Equal
  exception Maybe

  let rec _get_beta_redex env ctx term =
    match term with
    | TeVar _ -> raise Not_found
    | Cst(cst,_tys) -> raise Not_found
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

  let rec _get_redex (envl,envr) ctx (left,right) =
    (*
    Format.eprintf "left:%a@." (print__te envl) left;
    Format.eprintf "right:%a@." (print__te envr) right; *)
    match (left,right) with
    | TeVar _, TeVar _ -> raise Equal
    | Cst(cst,_tys), Cst(cst', _tys') ->
      if is_defined cst then
        if is_defined cst' then
          if cst = cst' then
            raise Maybe
          else
            (true, ctx, Delta(cst, _tys))
        else
          (true, ctx, Delta(cst,_tys))
      else if is_defined cst' then
        (false, ctx, Delta(cst',_tys'))
      else
        raise Equal
    | Abs (var, _ty, _tel), Abs (var', _ty', _ter) ->
      let envl' = add_te_var envl var _ty in
      let envr' = add_te_var envr var' _ty' in
      _get_redex (envl',envr') (CAbs::ctx) (_tel,_ter)
    | Forall (var, _ty, _tel), Forall (var', _ty', _ter) ->
      let envl' = add_te_var envl var _ty in
      let envr' = add_te_var envr var' _ty' in
      _get_redex (envl',envr') (CForall::ctx) (_tel,_ter)
    | Impl (_tel, _ter), Impl (_tel', _ter') ->
      begin
        try _get_redex (envl,envr) (CImplL::ctx) (_tel,_tel')
        with Equal | Maybe ->  _get_redex (envl,envr) (CImplR::ctx) (_ter,_ter')
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
      if is_redexable _tel then
        get_app_redex true ctx left
      else if is_redexable _tel' then
        get_app_redex false ctx right
      else
        begin
          try
            _get_redex (envl,envr) (CAppL::ctx) (_tel, _tel')
          with Equal ->
            _get_redex (envl,envr) (CAppR::ctx) (_ter,_ter')
        end
    | App _ , _ ->
      get_app_redex true ctx left
    | _, (App _) ->
      get_app_redex false ctx right
    | _ -> assert false

  let rec get_redex (envl,envr) ctx = function
    | Te left, Te right -> _get_redex (envl,envr) ctx (left,right)
    | ForallP(var,left), ForallP(var', right) ->
      let envl' = add_ty_var envl var in
      let envr' = add_ty_var envr var' in
      get_redex (envl',envr') (CForallP::ctx) (left,right)
    | _ -> assert false

  let get_redex env ctx lr =
    let is_left,ctx,redex = get_redex env ctx lr in
    is_left, List.rev ctx, redex

  let _get_redex env ctx lr =
    let is_left,ctx,redex = _get_redex env ctx lr in
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

  let newterm env ctx redex =
      match redex with
      | Beta(_te) ->
        let _tedk = Decompile.decompile__term env.dk _te in
        CTerm.compile__term env (Env.unsafe_reduction ~red:Strategy.beta_one _tedk)
      | Delta(cst,_tys) ->
        let name = name_of cst in
        let _tedk = Decompile.decompile__term env.dk (Cst(cst,_tys)) in
        (* These two steps might be buggy in the future since we use SNF instead of WHNF because of the coercion eps *)
        let _tedk' = Env.unsafe_reduction ~red:(Strategy.delta name) _tedk in
        let _tedk' = Env.unsafe_reduction ~red:(Strategy.beta_steps (List.length _tys)) _tedk' in
        CTerm.compile__term env _tedk'

  let _reduce env ctx redex _te =
    let newterm = newterm env ctx redex in
    _apply ctx newterm _te


  let reduce env ctx redex te =
    let newterm = newterm env ctx redex in
    apply ctx newterm te

  type 'a step =
    {
      is_left: bool;
      redex: redex;
      ctx: ctx list
    }

  let get_step is_left redex ctx = {is_left;redex;ctx}

  let _one_step env left right =
    let is_left,ctx,redex = _get_redex (env,env) [] (left,right) in
    if is_left then
      let env' = _env_of_redex env ctx left in
      let left' = _reduce env' ctx redex left in
      let step = get_step is_left redex ctx in
      step, left', right
    else
      let env' = _env_of_redex env ctx right in
      let right' = _reduce env' ctx redex right in
      let step = get_step is_left redex ctx in
      step, left, right'

  let one_step env left right =
    let is_left,ctx,redex = get_redex (env,env) [] (left,right) in
    if is_left then
      let env' = env_of_redex env ctx left in
      let left' = reduce env' ctx redex left in
      let step = get_step is_left redex ctx in
      step, left', right
    else
      let env' = env_of_redex env ctx right in
      let right' = reduce env' ctx redex right in
      let step = get_step is_left redex ctx in
      step, left, right'

  let empty_trace = {left= []; right = []}

  let rec _annotate_beta env _te =
    if _is_beta_normal env _te then
      [], _te
    else
      let ctx,redex = _get_beta_redex env [] _te in
      let env' = _env_of_redex env ctx _te in
      let _te' = _reduce env' ctx redex _te in
      let trace, _tenf = _annotate_beta env _te' in
      (redex, ctx)::trace, _tenf

  let rec annotate_beta env te =
    if is_beta_normal env te then
      [], te
    else
      let ctx,redex = get_beta_redex env [] te in
      let env' = env_of_redex env ctx te in
      let te' = reduce env' ctx redex te in
      let trace, tenf = annotate_beta env te' in
      (redex, ctx)::trace, tenf

  let rec _annotate env left right =
(*    Format.eprintf "left:%a@." (print__te env) left;
      Format.eprintf "right:%a@." (print__te env) right; *)
    if _eq env left right then
      empty_trace
    else
      let tracel,left' = _annotate_beta env left in
      let tracer, right' = _annotate_beta env right in
      let trace_beta = {left=tracel; right =tracer} in
      if _eq env left' right' then
        trace_beta
      else
        let step, left', right' = _one_step env left' right' in
        let trace' = _annotate env left' right' in
        let trace'' =
          if step.is_left then
            {trace' with left=(step.redex, step.ctx)::trace'.left}
          else
            {trace' with right=(step.redex, step.ctx)::trace'.right}
        in
        {left = trace_beta.left@trace''.left;
         right= trace_beta.right@trace''.right}

  let rec annotate env left right =
(*    Format.eprintf "left:%a@." (print_te env) left;
      Format.eprintf "right:%a@." (print_te env) right; *)
    if eq env left right then
      empty_trace
    else
      let tracel,left' = annotate_beta env left in
      let tracer, right' = annotate_beta env right in
      let trace_beta = {left=tracel; right =tracer} in
      if eq env left' right' then
        trace_beta
      else
        let step, left', right' = one_step env left' right' in
        let trace' = annotate env left' right' in
        let trace'' =
          if step.is_left then
            {trace' with left=(step.redex, step.ctx)::trace'.left}
          else
            {trace' with right=(step.redex, step.ctx)::trace'.right}
        in
        {left = trace_beta.left@trace''.left;
         right= trace_beta.right@trace''.right}
end
