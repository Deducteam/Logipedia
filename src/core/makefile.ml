(** Define the signature that must be implemented to obtain a build
    system. *)
open Extras

(** Build tools, provides keys, values, rules and generators to use
    the build system {!module:Build.Classic}. *)
module type S =
sig
  open Build.Classic

  type key
  (** Keys or targets. *)

  type value
  (** Yielded by the build of a key. *)

  val key_eq : key eq
  (** Equality on keys. *)

  val pp_key : key pp
  (** [pp_key fmt k] prints key [k] to formatter [fmt]. *)

  val valid_stored : key -> value -> bool
  (** [valid_stored k v] returns whether value [v] stored in the database is a
      valid value of key [k]. *)

  val want : string list -> key list
  (** [want p] creates targets out of paths [p]. Used to declare initial
      targets. *)

  val rules : (key, value) rule list
  (** Static rules. *)

  val generators : (key, value) generator list
  (** Dynamic rules generators. A generator takes a key as argument
      and returns a rule. They are used by the build system to create
      rules on the fly. Can be seen as a generalisation of the [%] in
      GNU make. *)
end
