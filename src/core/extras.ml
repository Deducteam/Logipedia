(** Type of equality function. *)
type 'a eq = 'a -> 'a -> bool

(** Type of a pretty printer. *)
type 'a pp = Format.formatter -> 'a -> unit

module Option =
struct
  type 'a t = 'a option

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f op ->
    match op with
    | None    -> None
    | Some(v) -> Some(f v)

  let get : 'a t -> 'a = function
    | Some(v) -> v
    | None    -> invalid_arg "Option.get"
end

module Unix =
struct
  include Unix

  (** [mkdir_rec s p] is like [mkdir -p s] setting file permissions [p]. *)
  let mkdir_rec : string -> file_perm -> unit = fun pth perm ->
    let reps = String.split_on_char '/' pth in
    let rec loop rem dir = match rem with
      | []    -> Unix.mkdir dir perm
      | p::tl ->
        if not (Sys.file_exists dir) then Unix.mkdir dir perm;
        loop tl (Filename.concat dir p)
    in
    loop reps "."
end

module String =
struct
  include String

  let drop : t -> int -> t = fun s start ->
    let len = length s in
    if start >= len then invalid_arg "String.drop" else
    sub s (start + 1) (len - start - 1)
end

module NameHashtbl = Hashtbl.Make(struct
    type t = Kernel.Basic.name
    let equal = Kernel.Basic.name_eq
    let hash = Hashtbl.hash
  end)
module NameMap = Map.Make(struct
    type t = Api.Dep.NameSet.elt
    let compare : t -> t -> int = Pervasives.compare
  end)
