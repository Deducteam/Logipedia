(* TODO make taxon and logic shared types *)
module T = Theories
module B = Basic

type taxon =
  | TxAxm (** Axiom *)
  | TxDef (** Definition *)
  | TxCst (** Constant *)
  | TxThm (** Theorem *)
  [@@deriving yojson]

(** The top level name (without modules nor taxon). *)
type name = string

(** Module system, each string should match [[_a-zA-Z]*] *)
type modu = string list

type t = T.theory * modu * name * taxon

let pp_modu : Format.formatter -> modu -> unit =
  fun fmt md ->
  let pp_sep fmt () = Format.fprintf fmt "/" in
  Format.pp_print_list ~pp_sep Format.pp_print_string fmt md

let string_of_taxon : ?short:bool -> taxon -> string = fun ?(short=false) tx ->
  match tx with
  | TxAxm -> if short then "axm" else "axiom"
  | TxDef -> if short then "def" else "definition"
  | TxCst -> if short then "cst" else "constant"
  | TxThm -> if short then "thm" else "theorem"

let pp : Format.formatter -> t -> unit = fun fmt (th, md, nm, tx) ->
  Format.fprintf fmt "%s:%a/%s.%s" (T.string_of_theory th) pp_modu md nm
    (string_of_taxon tx)

let to_string : t -> string = fun (th, md, nm, tx) ->
  (T.string_of_theory th) ^ ":" ^ String.concat "/" md ^ "/" ^ nm ^ "."
  ^ (string_of_taxon ~short:true tx)

let uri_of_dkid : B.mident -> B.ident -> T.theory -> taxon -> t =
  fun md id th tx ->
  (th, [B.string_of_mident md], B.string_of_ident id, tx)
