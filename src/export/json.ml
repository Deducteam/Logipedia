(** Export to json files. *)

module Jt = Json_types

let sys = "json"

let rec ppt_of__te : Ast._te -> Jt.Ppterm.t = fun t ->
  ppt_of__te_args t []

and ppt_of__ty : Ast._ty -> Jt.Ppterm.t = fun t ->
  ppt_of__ty_args t []

and ppt_of__ty_args : Ast._ty -> Ast._ty list -> Jt.Ppterm.t = fun t stk ->
  match t with
  | TyVar(v) -> Jt.Ppterm.Var(ppv_of_tyv v stk)
  | Arrow(m, n) -> ppt_of__ty_args m (n :: stk)
  | TyOp(_, _)  -> failwith "not implemented"
  | Prop        -> failwith "not implemented"

and ppt_of__te_args : Ast._te -> Ast._te list -> Jt.Ppterm.t = fun t stk ->
  match t with
  | TeVar(v)       -> Jt.Ppterm.Var(ppv_of_tev v stk)
  | Abs(v, ty, te) ->
     Jt.Ppterm.Binder
       { b_symb = "λ" ; bound = v ; annotation = Some(ppt_of__ty ty)
       ; body = ppt_of__te te }
  | App(t, u)      -> ppt_of__te_args t (u :: stk)
  | Forall(v, ty, te) ->
     Jt.Ppterm.Binder
       { b_symb = "∀" ; bound = v ; annotation = Some(ppt_of__ty ty)
       ; body = ppt_of__te te }
  | AbsTy(tyv, te) ->
     Jt.Ppterm.Binder
       { b_symb = "Λ" ; bound = tyv ; annotation = None ; body = ppt_of__te te }
  | Impl(_, _)     -> failwith "not implemented"
  | Cst(_, _)      -> failwith "not implemented"

and ppv_of_tyv : Ast.ty_var -> Ast._ty list -> Jt.Ppterm.var =
  fun v_symb args -> { v_symb ; v_args = List.map ppt_of__ty args }

let ppv_of_tev : Ast.te_var -> Ast._te list -> Jt.Ppterm.var =
  fun v_symb args -> { v_symb ; v_args = List.map ppt_of__te args }

let rec ppt_of_ty : Ast.ty -> Jt.Ppterm.t = function
  | ForallK(tyv, ty) ->
    Jt.Ppterm.Binder
      { b_symb = "∀" ; bound = tyv ; annotation = None
      ; body = ppt_of_ty ty}
  | Ty(ty)           -> ppt_of__ty ty

let rec ppt_of_te : Ast.te -> Jt.Ppterm.t = function
  | ForallP(tyv, te) ->
    Jt.Ppterm.Binder
      { b_symb = "∀" ; bound = tyv ; annotation = None
      ; body = ppt_of_te te }
  | Te(te)           -> ppt_of__te te

let print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit =
  fun _ ?mdeps:_ _ -> failwith "not implemented"

let pretty_print_item : Ast.item -> string = function
  | Definition(n, ty, te) ->
    let ppty = ppt_of_ty ty in
    let ppte = ppt_of_te te in
    Format.sprintf "Def %s: %s : %s"
      (snd n) (Yojson.Safe.pretty_to_string (Jt.Ppterm.to_yojson ppte))
      (Yojson.Safe.pretty_to_string (Jt.Ppterm.to_yojson ppty))
  | _ -> failwith "not implemented"
