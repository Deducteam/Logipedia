(** Signature of a system. *)

(** Ast and interactions with Dk files that a system must provide. *)
module type AST =
sig
  type t

  val compile : Kernel.Basic.mident -> Parsing.Entry.entry list -> t

  val decompile : t -> Parsing.Entry.entry list
end

(** Type of a system. *)
module type S =
sig
  module Ast : AST

  module Mid : Middleware.Middleware_types.S
  (** Middleware used for the json export. *)

  val export : Ast.t -> Format.formatter -> unit
  (** [export ast fmt] exports abstract syntax tree [ast] to formatter
      [fmt] in the syntax of the system. *)
end
