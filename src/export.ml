type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean ]

module type E =
sig
  val extension: string
  val print_ast : out_channel -> string -> Ast.ast -> unit
end

module type BDD =
sig
  include E
  val print_bdd : Ast.ast -> unit
end

module PVS : E =
struct
  let extension = "pvs"
  let print_ast = Pvs.print_ast
end

module COQ : E =
struct
  let extension = "v"
  let print_ast = Coq.print_ast
end

module MATITA : E =
struct
  let extension = "ma"
  let print_ast = Matita.print_ast
end

module OPENTHEORY : E =
struct
  let extension = "art"
  let print_ast = Opentheory.print_ast
end

module LEAN : E =
struct
  let extension = "lean"
  let print_ast = Lean.print_ast
end

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs        -> (module PVS)
  | `Coq        -> (module COQ)
  | `Matita     -> (module MATITA)
  | `OpenTheory -> (module OPENTHEORY)
  | `Lean       -> (module LEAN)
