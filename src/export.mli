module type E =
sig
  val system         : Systems.system
  val extension      : string
  val print_ast      : Format.formatter -> Ast.ast -> unit
  val print_bdd      : Ast.ast -> unit
end

val of_system : Systems.system -> (module E)
