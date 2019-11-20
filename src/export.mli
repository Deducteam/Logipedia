(** Functions to write files in a foreign system.  Available systems
    are the ones described by the type {!type:Systems.system}. *)

open Kernel
open Parsing

(** E is the signature for an exporting system. *)
module type E =
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

val of_system : Systems.system -> (module E)

(* FIXME: STTfa? And the others? *)
(** [mk_ast md es] creates the STTfa ast for module [md]. *)
val mk_ast : Basic.mident -> Entry.entry list -> Ast.ast

(** [export_system Exp f] exports file [f] using module [Exp]. *)
val export_system : (module E) -> string -> Format.formatter -> unit
