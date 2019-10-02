(** Types of data used in json files.  We use extensively the yojson
    deriver which automatically create, for a type [t], two functions
    [t_to_yojson: t -> Yojson.Safe.t] and
    [t_of_yojson : Yojson.Safe.t -> t]. *)

(* Should the names be uris as strings or uris as themselves? *)

let json_dir : string ref = ref ""
(** Directory containing json files. *)

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

(** An item is more or less a Dedukti entry, with additional information. *)
type item =
  { name : string
  (** A fully qualified name, the representation of a Uri. *)
  ; taxonomy : string
  ; term : Ppterm.t
  (** Meaning is given by the taxonomy. *)
  ; term_opt : Ppterm.t option
  (** Depends on the item taxonomy. *)
  ; label : string * string option
  (** Meaning of {!field:term} and {!field:term_opt}. *)
  ; deps : string list
  (** Direct dependencies of the item (no transitive closure). *)
  ; theory : string list
  ; exp : string list
  (** Available export systems. *) }
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
