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

module List = struct
  include List

  (** [mem_eq eq e l] is [List.mem e l] with equality function [eq]. *)
  let rec mem_eq : 'a eq -> 'a -> 'a list -> bool = fun eq elt l ->
    match l with
    | []                 -> false
    | x::_ when eq x elt -> true
    | _::l               -> mem_eq eq elt l

  let rec uniq_eq : 'a eq -> 'a list -> 'a list = fun eq l ->
    match l with
    | []                      -> []
    | x::t when mem_eq eq x t -> uniq_eq eq t
    | x::t                    -> x :: uniq_eq eq t

  let rec remove_eq : 'a eq -> 'a -> 'a list -> 'a list = fun eq elt l ->
    match l with
    | []                 -> []
    | x::t when eq x elt -> t
    | x::t               -> x :: remove_eq eq elt t
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

module Filename =
struct
  include Filename
  type t = string

  (** [t <.> u] returns path [t.u]. *)
  let ( <.> ) : t -> t -> t = fun t u -> t ^ "." ^ u

  (** [t </> u] is an alias for [concat]. *)
  let ( </> ) : t -> t -> t = concat

  (** [!/ t] removes the extension of [t] and takes the basename, that is,
      [!/"import/leibniz.dk" = "leibniz"]. *)
  let ( !/ ) : t -> t = fun t -> chop_extension t |> basename
end

(** [mtime p] returns the modification time of a file. *)
let mtime : string -> float = fun string -> Unix.((stat string).st_mtime)

(** [atime p] returns the modification time of a file. *)
let atime : string -> float = fun string -> Unix.((stat string).st_atime)

(** Some handy Dedukti functions or aliases. *)
module DkTools = struct
  type mident = Kernel.Basic.mident
  let mident_eq : mident eq = Kernel.Basic.mident_eq
  let pp_mident : mident pp = Api.Pp.Default.print_mident

  let get_file : mident -> string = Api.Dep.get_file
  let init : string -> mident = Api.Env.Default.init
  type entry = Parsing.Entry.entry

  let get_path : unit -> string list = Kernel.Basic.get_path
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

(** [memoize f] returns the function [f] memoized using pervasive equality. *)
let memoize (type arg) : ?eq:arg eq -> (arg -> 'b) -> arg -> 'b = fun ?eq f ->
  let module ArH = Hashtbl.Make(struct
      type t = arg
      let equal = match eq with None -> Stdlib.(=) | Some(eq) -> eq
      let hash = Stdlib.Hashtbl.hash
    end)
  in
  let memo = ArH.create 19 in
  fun x ->
    try ArH.find memo x with Not_found ->
    let r = f x in
    ArH.add memo x r;
    r
