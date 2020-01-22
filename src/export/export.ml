(** Signature of an export system must comply with the signature
    {!val:S} defined here. *)

open Core
open Extras

(** Export system must have this signature. *)
module type S =
sig
  type ast

  val target : Systems.t
  (** Which target system to export to. *)

  val compile : Kernel.Basic.mident -> Parsing.Entry.entry list -> ast
  (** [compile md es] builds an ast out of a list of Dedukti entries
      [es] coming from module [md]. *)

  val decompile : ast -> Parsing.Entry.entry list
  (** [decompile ast] returns the list of Dedukti entries coming from
      ast [ast]. *)

  val export : ast pp
  (** [export fmt ast] exports abstract syntax tree [ast] to formatter
      [fmt] in the syntax of the system. *)
end
