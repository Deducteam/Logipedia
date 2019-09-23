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

module List =
struct
  include List

  (** [filter_map f y] like a [List.map], discarding elements for
      which [f x] is None. *)
  let rec filter_map : ('a -> 'b option) -> 'a list -> 'b list =
    fun f xs ->
    match xs with
    | [] -> []
    | h :: tl ->
      match f h with
      | Some(v) -> v :: filter_map f tl
      | None    -> filter_map f tl
end

module String =
struct
  include String

  let drop : t -> int -> t = fun s start ->
    let len = length s in
    if start >= len then invalid_arg "String.drop" else
    sub s (start + 1) (len - start - 1)
end

module StrMap = Map.Make(String)
module Str2Map = Map.Make(struct
    type t = string * string
    let compare = Pervasives.compare end)

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
