(** Signature of a system. *)

(** Ast and interactions with Dk files that a system must provide. *)
module type AST =
sig
  type t
  val compile : 'a -> t
  val decompile : t -> 'a
end

(** Type of a system. *)
module type S =
sig
  module Ast : AST

  module Mid : Middleware.S
  (** Middleware used for the json export. *)

  val export : Ast.t -> Format.formatter -> unit
  (** [export ast fmt] exports abstract syntax tree [ast] to formatter
      [fmt] in the syntax of the system. *)
end
