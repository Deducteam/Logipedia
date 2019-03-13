(** E is the signature for an exporting system. *)
module type E =
sig
  val system            : Systems.system
  val extension         : string
  val print_ast         : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit
  val pretty_print_item : Ast.item -> string
end

val of_system : Systems.system -> (module E)
