type id = string [@@deriving yojson]
type qid = id list [@@deriving yojson]
type var =
  { v_symb: id
  ; v_args: ppterm list }
  [@@deriving yojson]
and const =
  { c_symb: qid
  ; c_args: ppterm list }
  [@@deriving yojson]
and binder =
  { b_symb: id
  ; bound: id
  ; annotation: ppterm option
  ; body: ppterm }
  [@@deriving yojson]
and ppterm =
  | Var of var
  | Binder of binder
  | Const of const
[@@deriving yojson]
