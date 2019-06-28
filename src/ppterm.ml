type id = string [@@deriving yojson]
type qid = id list [@@deriving yojson]
type db =
  { index: int
  ; db_args: ppterm list }
  [@@deriving yojson]
and binder =
  { symbol: id
  ; bound: id
  ; annotation: ppterm option
  ; body: ppterm }
  [@@deriving yojson]
and const =
  { c_id: qid
  ; c_args: ppterm list }
  [@@deriving yojson]
and ppterm =
  | DB of db
  | Binder of binder
  | Const of const
[@@deriving yojson]
