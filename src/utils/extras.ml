module Option =
struct
  type 'a t = 'a option

  (** [map f o] is a monadic bind: computes [f v] if [o] is a value
      [v] (that is, [Some(v)]), or returns [None]. *)
  let map : ('a -> 'b) -> 'a t -> 'b t = fun f op ->
    match op with
    | None    -> None
    | Some(v) -> Some(f v)
end

module String =
struct
  include String

  let drop : t -> int -> t = fun s start ->
    let len = length s in
    if start >= len then invalid_arg "String.drop" else
    sub s (start + 1) (len - start - 1)
end

module Basic =
struct
  include Basic
  module NameHashtbl = Hashtbl.Make(struct
      type t = Basic.name
      let equal = Basic.name_eq
      let hash = Hashtbl.hash
    end)
end

module Dep =
struct
  include Dep

  module NameMap = Map.Make(struct
      type t = Dep.NameSet.elt
      let compare : t -> t -> int = Pervasives.compare
    end)
end
