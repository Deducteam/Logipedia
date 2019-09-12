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
  | Arrow(m, n) ->
    let pptm = ppt_of__ty m in
    let pptn = ppt_of__ty n in
    Jt.Ppterm.Const { c_symb = ["→"] ; c_args = [pptm; pptn] }
  | TyOp((p,id), tys) ->
    Jt.Ppterm.Const { c_symb = [p; id]
                    ; c_args = List.map ppt_of__ty (tys @ stk) }
  | Prop -> Jt.Ppterm.Const { c_symb = ["Prop"]
                            (* c_args should probably be empty *)
                            ; c_args = List.map ppt_of__ty stk }

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
  | Impl(t, u)            ->
    Jt.Ppterm.Const { c_symb = ["⇒"]
                    ; c_args = List.map ppt_of__te (t::u::stk) }
  | Cst((p,id), _)        ->
    Jt.Ppterm.Const { c_symb = [p; id] ; c_args = List.map ppt_of__te stk }

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

let jsitem_of_item : Ast.item -> Jt.item =
  (* FIXME many constructions are erroneous. *)
  function
  | Definition((_,id), ty, te) ->
    let ppty = ppt_of_ty ty in
    let ppte = ppt_of_te te in
    { name = id ; taxonomy = "definition" ; term = ppte ; body = ppty
    ; deps = [] ; theory = [] ; exp = [] }
  | Axiom((_,id), te)          ->
    let ppte = ppt_of_te te in
    { name = id ; taxonomy = "axiom" ; term = ppte ; body = ppte
    ; deps = [] ; theory = [] ; exp = [] }
  | Theorem((_,id), te, _)     ->
    let ppte = ppt_of_te te in
    { name = id ; taxonomy = "axiom" ; term = ppte ; body = ppte
    ; deps = [] ; theory = [] ; exp = [] }
  | TypeDef((_,id), _, _ty)    ->
    let ppty = ppt_of__ty _ty in
    { name = id ; taxonomy = "definition" ; term = ppty ; body = ppty
    ; deps = [] ; theory = [] ; exp = [] }
  | TypeDecl((_,id), _)        ->
    let dummy = Jt.Ppterm.Var { v_symb = "dummy" ; v_args = [] } in
    { name = id ; taxonomy = "definition" ; term = dummy ; body = dummy
    ; deps = [] ; theory = [] ; exp = [] }
  | Parameter((_,id), ty)      ->
    let ppty = ppt_of_ty ty in
    { name = id ; taxonomy = "parameter" ; term = ppty ; body = ppty
    ; deps = [] ; theory = [] ; exp = [] }

let pretty_print_item : Ast.item -> string = fun it ->
  jsitem_of_item it |> Jt.item_to_yojson |> Yojson.Safe.pretty_to_string

let print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit =
  fun fmt ?mdeps:_ { md = _ ; dep = _ ; items } ->
  let js_items = List.map jsitem_of_item items in
  Yojson.Safe.pretty_print fmt (Jt.document_to_yojson js_items)
