open Kernel
open Parsing

module QSet : Set.S with type elt = string
(** [dep_of_entry mds e] compute the direct dependencies of [e] which are not part of [mds]. *)
val dep_of_entry : Basic.mident list -> Entry.entry -> QSet.t
