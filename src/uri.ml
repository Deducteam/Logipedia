(* TODO make taxon and logic shared types *)
module T = Theories
module B = Basic
module Tx = Taxonomy

(** The top level name (without modules nor taxon). *)
type name = string

(** Module system, each string should match [[_a-zA-Z]*] *)
type modu = string list

type t = T.theory * modu * name * Tx.Sttfa.t

let to_string : t -> string = fun (th, md, nm, tx) ->
  (T.string_of_theory th) ^ ":" ^ String.concat "/" md ^ "/" ^ nm ^ "."
  ^ (Tx.Sttfa.to_string ~short:true tx)

let uri_of_dkid : B.mident -> B.ident -> T.theory -> Tx.Sttfa.t-> t =
  fun md id th tx ->
  (th, [B.string_of_mident md], B.string_of_ident id, tx)
