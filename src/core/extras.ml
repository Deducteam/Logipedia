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
    if Filename.is_relative pth then loop reps "." else loop reps "/"
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

  (** [~. t] prefixes [t] with a dot if it does not begin with one. *)
  let ( ~. ) : t -> t = fun t ->
    if String.get t 0 = '.' then t else "." ^ t

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
  module Mident = struct
    open Kernel.Basic
    type t = mident
    let equal : t eq = mident_eq
    let compare : t -> t -> int = fun m n ->
      String.compare (string_of_mident m) (string_of_mident n)
    let pp : t pp = Api.Pp.Default.print_mident
  end

  (** Functional sets of module identifiers. *)
  module MdSet = Set.Make(Mident)
  (* TODO use Api.Dep.MDepSet everywhere? *)

  let get_file : Mident.t -> string = Api.Files.get_file
  let init : string -> Mident.t = Kernel.Basic.mk_mident

  type entry = Parsers.Entry.entry

  let get_path : unit -> string list = Api.Files.get_path

  let id_of_entry : Parsers.Entry.entry -> Kernel.Basic.ident =
    let open Parsers.Entry in
    function
    | Decl(_,id,_,_,_) -> id
    | Def(_,id,_,_,_,_) -> id
    | Rules(_,r::_) ->
      begin
        match r.pat with
        | Pattern(_,n,_) -> Kernel.Basic.id n
        | _              -> assert false
        (* the head of a rule should be a symbol *)
      end
    | _               -> assert false
    (* tx_of_entry should be defined for more than Def, Decl and Rules *)

  let is_static : Kernel.Signature.t -> Kernel.Basic.loc -> Kernel.Basic.name ->
    bool = fun sign l n ->
    Kernel.Signature.get_staticity sign l n = Kernel.Signature.Static
end

module NameHashtbl = Hashtbl.Make(struct
    type t = Kernel.Basic.name
    let equal = Kernel.Basic.name_eq
    let hash = Hashtbl.hash
  end)
module NameMap = Map.Make(struct
    type t = Kernel.Basic.NameSet.elt
    let compare : t -> t -> int = Stdlib.compare
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
