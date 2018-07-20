type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Sttfa]

module type E =
sig
  val system    : system
  val extension : string
  val print_ast : Format.formatter -> string -> Ast.ast -> unit
  val print_bdd : Ast.ast -> unit
end
(*
module type JSON =
sig
  val print_json : Ast.ast -> unit
end

module MAKE_JSON : functor (THEORY:E) -> JSON
                                         *)
val of_system : system -> (module E)
