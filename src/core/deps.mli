open Kernel
open Parsing

module QSet : Set.S with type elt = string
(** Sets of strings. *)

val dep_of_entry : Basic.mident list -> Entry.entry -> QSet.t
(** [dep_of_entry mds e] compute the direct dependencies of [e] which
    are not part of [mds]. *)

val deps_of_md : Basic.mident -> Basic.mident list
(** [deps_of_md md] returns the list of modules id on which [md] from input. *)
