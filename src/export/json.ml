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
  | TeVar(v_symb)         ->
    Jt.Ppterm.Var { v_symb ; v_args = List.map ppt_of__te stk }
  | Abs(bound, ty, te)    ->
     Jt.Ppterm.Binder
       { b_symb = "λ" ; bound ; annotation = Some(ppt_of__ty ty)
       ; body = ppt_of__te te }
  | App(t, u)      -> ppt_of__te_args t (u :: stk)
  | Forall(bound, ty, te) ->
     Jt.Ppterm.Binder
       { b_symb = "∀" ; bound ; annotation = Some(ppt_of__ty ty)
       ; body = ppt_of__te te }
  | AbsTy(bound, te)      ->
     Jt.Ppterm.Binder
       { b_symb = "Λ" ; bound ; annotation = None ; body = ppt_of__te te }
  | Impl(_, _)            -> failwith "not implemented"
  | Cst(_, _)             -> failwith "not implemented"

and ppv_of_tyv : Ast.ty_var -> Ast._ty list -> Jt.Ppterm.var =
  fun v_symb args -> { v_symb ; v_args = List.map ppt_of__ty args }

let ppv_of_tev : Ast.te_var -> Ast._te list -> Jt.Ppterm.var =
  fun v_symb args -> { v_symb ; v_args = List.map ppt_of__te args }

let rec ppt_of_ty : Ast.ty -> Jt.Ppterm.t = function
  | ForallK(bound, ty) ->
    Jt.Ppterm.Binder
      { b_symb = "∀" ; bound ; annotation = None ; body = ppt_of_ty ty}
  | Ty(ty)             -> ppt_of__ty ty

let rec ppt_of_te : Ast.te -> Jt.Ppterm.t = function
  | ForallP(bound, te) ->
    Jt.Ppterm.Binder
      { b_symb = "∀" ; bound ; annotation = None ; body = ppt_of_te te }
  | Te(te)             -> ppt_of__te te

let string_of_ppt : Jt.Ppterm.t -> string = fun x ->
  Jt.Ppterm.to_yojson x |> Yojson.Safe.pretty_to_string

let pretty_print_item : Ast.item -> string = function
  | Parameter(n, ty)      ->
    let ppty = ppt_of_ty ty |> string_of_ppt in
    Format.sprintf "Parameter %s: %s" (snd n) ppty
  | Definition(n, ty, te) ->
    let ppty = ppt_of_ty ty |> string_of_ppt in
    let ppte = ppt_of_te te |> string_of_ppt in
    Format.sprintf "Def %s: %s : %s" (snd n) ppte ppty
  | Axiom(n, te)          ->
    let ppte = ppt_of_te te |> string_of_ppt in
    Format.sprintf "Axiom %s: %s" (snd n) ppte
  | _ -> failwith "not implemented"

let print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit =
  fun fmt ?mdeps:_ { md = _ ; dep = _ ; items } ->
  let pp_sep fmt () = Format.fprintf fmt "\n" in
  let pp_item fmt it = Format.fprintf fmt "%s" (pretty_print_item it) in
  Format.pp_print_list ~pp_sep pp_item fmt items
