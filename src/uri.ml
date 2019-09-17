(* TODO make taxon and logic shared types *)
module T = Theories

type taxon =
  | TxAxm (** Axiom *)
  | TxDef (** Definition *)
  | TxCst (** Constant *)
  | TxThm (** Theorem *)

(** The top level name (without modules nor taxon). *)
type name = string

(** Module system, each string should match [[_a-zA-Z]*] *)
type modu = string list

type t = T.theory * modu * name * taxon

let pp_modu : Format.formatter -> modu -> unit =
  fun fmt md ->
  let pp_sep fmt () = Format.fprintf fmt "/" in
  Format.pp_print_list ~pp_sep Format.pp_print_string fmt md

let string_of_taxon : taxon -> string = function
  | TxAxm -> "axiom"
  | TxDef -> "definition"
  | TxCst -> "constant"
  | TxThm -> "theorem"

let pp : Format.formatter -> t -> unit = fun fmt (th, md, nm, tx) ->
  Format.fprintf fmt "%s:%a/%s.%s" (T.string_of_theory th) pp_modu md nm
    (string_of_taxon tx)
