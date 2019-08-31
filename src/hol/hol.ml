open Basic

let hol_module = mk_mident "hol"

let hol_type = mk_ident "type"
let hol_bool = mk_ident "bool"
let hol_arr = mk_ident "arr"

let hol_term = mk_ident "term"
let hol_eq = mk_ident "eq"

let is_hol_const c t =
  match t with
  | Term.Const(_, cst) -> name_eq cst (mk_name hol_module c)
  | _ -> false
