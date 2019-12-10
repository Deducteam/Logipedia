open Kernel
open Parsing


(** {1 Build library} based on Shake. *)

(** Resources: what is built. *)
module type Resources =
sig
  (** Targets *)
  type key

  (** Content built. *)
  type value
end

(** An available build system. *)
module type BuildSys =
sig
  type key
  type value

  (** How to build a target. *)
  type action

  (** Defines the creation of a target via an action. *)
  type rule

  (** [build rules target] builds target [target] with rules [rules]. *)
  val build : rule list -> key -> value
end

(** [Make R] creates a build system from resources [R]. *)
module Make : functor (R:Resources) -> BuildSys
  with type key = R.key
   and type value = R.value

module QSet : Set.S with type elt = string
(** Sets of strings. *)

val dep_of_entry : Basic.mident list -> Entry.entry -> QSet.t
(** [dep_of_entry mds e] compute the direct dependencies of [e] which
    are not part of [mds]. *)
