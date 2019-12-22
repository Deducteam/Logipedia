(** Signature of a system. *)

open Core
open Extras

(** Ast and interactions with Dk files that a system must provide. *)
module type AST =
sig
  type t

  val compile : Kernel.Basic.mident -> Parsing.Entry.entry list -> t
  (** [compile md es] builds an ast out of a list of Dedukti entries
      [es] coming from module [md]. *)

  val decompile : t -> Parsing.Entry.entry list
  (** [decompile ast] returns the list of Dedukti entries coming from
      ast [ast]. *)
end

(** Type of a system. *)
module type S =
sig
  module Ast : AST

  module Mid : Middleware.S
  (** Middleware used for the json export. *)

  module Makefile : Build_template.S
  (** Defines the rules to build targets. *)

  val export : Ast.t pp
  (** [export fmt ast] exports abstract syntax tree [ast] to formatter
      [fmt] in the syntax of the system. *)
end
