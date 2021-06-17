(** Functions to write files in a foreign system.  Available systems
    are the ones described by the type {!type:Systems.system}. *)
open Core

(** Directly exports from STTfa to the given system as a string *)
val export_to_system_as_string : Api.Env.t -> Systems.t -> Ast.item -> string

(** Generate an Exporter module from STTfa to the given system.
    This allows to handle export through the Makefile system. *)
val get_sttfa_exporter : Api.Env.t -> Systems.t -> (module Export.S)
