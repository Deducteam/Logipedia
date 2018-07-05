type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Sttfa ]

module type E =
sig
  val extension: string
  val print_ast : Format.formatter -> string -> Ast.ast -> unit
  val print_bdd : Ast.ast -> unit
end

val of_system : system -> (module E)
