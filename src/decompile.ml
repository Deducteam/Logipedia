open Sttforall
open Ast
open Basic
open Term

let to_const ident = Term.mk_Const dloc (mk_name sttfa_module ident)

let to_name (md, id) = mk_name (mk_mident md) (mk_ident id)

let rec find name ctx i =
  match ctx with
  | [] -> assert false
  | x :: t -> if name = x then i else find name t (i + 1)


let find name ctx = find name ctx 0

let rec decompile__type k env _ty =
  match _ty with
  | Prop -> to_const sttfa_prop
  | Arrow (_tyl, _tyr) ->
      Term.mk_App (to_const sttfa_arrow)
        (decompile__type k env _tyl)
        [decompile__type k env _tyr]
  | TyVar string -> Term.mk_DB dloc (mk_ident string) (find string env)
  | TyOp (tyop, []) -> Term.mk_Const dloc (to_name tyop)
  | TyOp (tyop, _ty :: _args) ->
      Term.mk_App
        (Term.mk_Const dloc (to_name tyop))
        (decompile__type k env _ty)
        (List.map (decompile__type k env) _args)


let rec decompile_type k env ty =
  match ty with
  | Ty ty -> decompile__type k env ty
  | ForallK (ty_var, ty) ->
      let ty' = decompile_type (k + 1) (ty_var :: env) ty in
      Term.mk_App
        (to_const sttfa_forall_kind_type)
        (Term.mk_Lam dloc (mk_ident ty_var) (Some (to_const sttfa_type)) ty')
        []


let to__type _ty =
  Term.mk_App (to_const sttfa_etap) (Term.mk_App (to_const sttfa_p) _ty []) []


(* ASSUMPTION: the set of ty_var and te_var are disjoint *)
let rec decompile__term k env _te =
  match _te with
  | TeVar x -> Term.mk_DB dloc (mk_ident x) (find x env)
  | Abs (x, _ty, _te) ->
      let env' = x :: env in
      Term.mk_Lam dloc (mk_ident x)
        (Some (to__type (decompile__type k env _ty)))
        (decompile__term (k + 1) env' _te)
  | App (tel, ter) ->
      Term.mk_App (decompile__term k env tel) (decompile__term k env ter) []
  | Forall (te_var, _ty, _te) ->
      let env' = te_var :: env in
      let _ty' = decompile__type k env _ty in
      let _te' = decompile__term (k + 1) env' _te in
      Term.mk_App (to_const sttfa_forall) _ty'
        [Term.mk_Lam dloc (mk_ident te_var) (Some (to__type _ty')) _te']
  | Impl (_tel, _ter) ->
      let _tel' = decompile__term (k + 1) env _tel in
      let _ter' = decompile__term (k + 1) env _ter in
      Term.mk_App (to_const sttfa_impl) _tel' [_ter']
  | AbsTy (ty_var, _te) ->
      let env' = ty_var :: env in
      Term.mk_Lam dloc (mk_ident ty_var) (Some (to_const sttfa_type))
        (decompile__term (k + 1) env' _te)
  | Cst (cst, []) -> Term.mk_Const dloc (to_name cst)
  | Cst (cst, x :: args) ->
      let x' = decompile__type k env x in
      let args' = List.map (decompile__type k env) args in
      Term.mk_App (Term.mk_Const dloc (to_name cst)) x' args'


let rec decompile_term k env te =
  match te with
  | Te _te -> decompile__term k env _te
  | ForallP (ty_var, te) ->
      let te' = decompile_term (k + 1) (ty_var :: env) te in
      Term.mk_App
        (to_const sttfa_forall_kind_prop)
        (Term.mk_Lam dloc (mk_ident ty_var) (Some (to_const sttfa_type)) te')
        []
