type var =
  { v_symb: string
  ; v_args: ppterm list }
  [@@deriving yojson]
and const =
  { c_symb: string list
  ; c_args: ppterm list }
  [@@deriving yojson]
and binder =
  { b_symb: string
  ; bound: string
  ; annotation: ppterm option
  ; body: ppterm }
  [@@deriving yojson]
and ppterm =
  | Var of var
  | Binder of binder
  | Const of const
[@@deriving yojson]
