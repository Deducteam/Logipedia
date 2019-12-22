(** Some rule making facilities. *)
open Extras
open Build.Classic

(** Some shorthands. *)
type mident = Kernel.Basic.mident
let mident_eq : mident eq = Kernel.Basic.mident_eq
type entry = Parsing.Entry.entry

(** Module taking in charge export of modules. *)
module type S =
sig
  type key
  type value

  val key_eq : key eq
  val pp_key : key pp
  val valid_stored : key -> value -> bool
  val want : string -> key
  val rules_for : mident -> string -> (key, value) rule list
end
