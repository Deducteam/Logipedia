open Systems

module type E =
sig
  val system         : Systems.system
  val extension      : string
  val print_ast      : Format.formatter -> Ast.ast -> unit
  val print_bdd      : Ast.ast -> unit
end

module PVS : E =
struct
  let system    = `Pvs
  let extension = "pvs"
  let print_ast = Pvs.print_ast
  let print_bdd = Pvs.print_bdd
end

module COQ : E =
struct
  let system    = `Coq
  let extension = "v"
  let print_ast = Coq.print_ast
  let print_bdd = Coq.print_bdd
end

module MATITA : E =
struct
  let system    = `Matita
  let extension = "ma"
  let print_ast = Matita.print_ast
  let print_bdd = Matita.print_bdd
end

module OPENTHEORY : E =
struct
  let system    = `OpenTheory
  let extension = "art"
  let print_ast = Opentheory.print_ast
  let print_bdd = Opentheory.print_bdd
end

module LEAN : E =
struct
  let system    = `Lean
  let extension = "lean"
  let print_ast = Lean.print_ast
  let print_bdd = Lean.print_bdd
end

module STTFA : E =
struct
  let system    = `Sttfa
  let extension = "dk"
  let print_ast = Sttfa.print_ast
  let print_bdd = Sttfa.print_bdd
end

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs        -> (module PVS)
  | `Coq        -> (module COQ)
  | `Matita     -> (module MATITA)
  | `OpenTheory -> (module OPENTHEORY)
  | `Lean       -> (module LEAN)
  | `Sttfa    -> (module STTFA)
