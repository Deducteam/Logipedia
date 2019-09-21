module B = Basic
module Tx = Taxonomy

(** The top level name (without modules nor taxon). *)
type name = string

(** Module system, each string should match [[_a-zA-Z]*] *)
type modu = string list

type t = string * modu * name * Tx.Sttfa.t

let to_string : t -> string = fun (th, md, nm, tx) ->
  th ^ ":" ^ String.concat "/" md ^ "/" ^ nm ^ "."
  ^ (Tx.Sttfa.to_string ~short:true tx)

let uri_of_dkid : B.mident -> B.ident -> string -> Tx.Sttfa.t-> t =
  fun md id th tx ->
  (th, [B.string_of_mident md], B.string_of_ident id, tx)
