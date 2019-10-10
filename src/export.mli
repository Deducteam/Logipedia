(** Functions to write files in a foreign system.  Available systems
    are the ones described by the type {!type:Systems.system}. *)

(** E is the signature for an exporting system. *)
module type Eo =
sig
  val system            : Systems.system
  (** System identifier. *)

  val extension         : string
  (** Extension of the files from this system. *)

  val print_ast         : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast
    -> unit
  (** [print_ast fmt ?mdeps ast] prints Sttfa abstract syntax tree
      [ast] to formatter [fmt]
      TODO document ?mdeps. *)

  val string_of_item : Ast.item -> string
end

val mk_exporter : Api.Env.t -> Systems.system -> (module Eo)
