type system = [`Coq | `Matita | `Pvs | `Latex | `Csv | `OpenTheory ]

module type E =
sig
  val extension: string
  val print_ast : out_channel -> string -> Ast.ast -> unit
end

val of_system : system -> (module E)
