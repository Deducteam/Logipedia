(** Types of data used in json files.  We use extensively the yojson
    deriver which automatically create, for a type [t], two functions
    [t_to_yojson: t -> Yojson.Safe.t] and
    [t_of_yojson : Yojson.Safe.t -> t]. *)

(* Should the names be uris as strings or uris as themselves? *)

module Ppterm =
struct
  type var =
    { v_symb: string
    ; v_args: t list }
  [@@deriving yojson]
  and const =
    { c_symb: string
    ; c_args: t list }
  [@@deriving yojson]
  and binder =
    { b_symb: string
    ; bound: string
    ; annotation: t option
    ; body: t
    ; b_args : t list
    (** Some terms are not in β normal form. *) }
  [@@deriving yojson]
  and t =
    | Var of var
    | Binder of binder
    | Const of const
  [@@deriving yojson]
end

type dependency = string list [@@deriving yojson]

type item =
  { name : string
  ; taxonomy : Uri.taxon
  ; term : Ppterm.t option
  ; body : Ppterm.t
  ; deps : dependency list
  ; theory : string list
  ; exp : string list
  (** Available systems. *) }
  [@@deriving yojson]

type export =
  { system : string
  (** Name of the foreign proof system. *)
  ; file : string
  (** Path of file. *)
  ; etype : string option
  (** A textual representation of the element in the foreign system *) }
  [@@deriving yojson]

type document = item list
[@@deriving yojson]
