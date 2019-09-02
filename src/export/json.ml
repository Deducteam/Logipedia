(** Export to json files. *)

let sys = "json"

let ppv_of_tyv : Ast.ty_var -> Ast._ty list -> Ppterm.var =
  failwith "not implemented"

let ppv_of_tev : Ast.te_var -> Ast._te list -> Ppterm.var =
  failwith "not implemented"

let rec ppt_of__te : Ast._te -> Ppterm.ppterm = fun t ->
  ppt_of__te_args t []

and ppt_of__ty : Ast._ty -> Ppterm.ppterm = fun t ->
  ppt_of__ty_args t []

and ppt_of__ty_args : Ast._ty -> Ast._ty list -> Ppterm.ppterm = fun t stk ->
  match t with
  | TyVar(v) -> Ppterm.Var(ppv_of_tyv v stk)
  | Arrow(m, n) -> ppt_of__ty_args m (n :: stk)
  | TyOp(_, _)  -> failwith "not implemented"
  | Prop        -> failwith "not implemented"

and ppt_of__te_args : Ast._te -> Ast._te list -> Ppterm.ppterm = fun t stk ->
  match t with
  | TeVar(v)       -> Ppterm.Var(ppv_of_tev v stk)
  | Abs(v, ty, te) ->
     Ppterm.Binder
       { b_symb = "λ" ; bound = v ; annotation = Some(ppt_of__ty ty)
       ; body = ppt_of__te te }
  | App(t, u)      -> ppt_of__te_args t (u :: stk)
  | Forall(v, ty, te) ->
     Ppterm.Binder
       { b_symb = "∀" ; bound = v ; annotation = Some(ppt_of__ty ty)
       ; body = ppt_of__te te }
  | Impl(t, u)     ->
     Ppterm.Binder
       { b_symb = "∀" ; bound = "_" ; annotation = Some(ppt_of__te t)
       ; body = ppt_of__te u }
  | Cst(_, _)    -> assert false
  | AbsTy(tyv, te) ->
     Ppterm.Binder
       { b_symb = "Λ" ; bound = tyv ; annotation = None ; body = ppt_of__te te }

let print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit =
  fun _ ?mdeps _ -> assert false

let pretty_print_item : Ast.item -> string = fun _ -> assert false
