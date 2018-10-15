open Systems

module type E =
sig
  val system         : Systems.system
  val extension      : string
  val print_ast      : Format.formatter -> Ast.ast -> unit
  val print_meta_ast : Format.formatter -> Ast.meta_ast -> unit
  val print_bdd      : Ast.ast -> unit
end

module PVS : E =
struct
  include Pvs
  let system    = `Pvs
  let extension = "pvs"
end

module COQ : E =
struct
  include Coq
  let system    = `Coq
  let extension = "v"
end

module MATITA : E =
struct
  include Matita
  let system    = `Matita
  let extension = "ma"
end

module OPENTHEORY : E =
struct
  include Opentheory
  let system    = `OpenTheory
  let extension = "art"
end

module LEAN : E =
struct
  include Lean
  let system    = `Lean
  let extension = "lean"
end

module STTFA : E =
struct
  include Sttfa
  let system    = `Sttfa
  let extension = "dk"
end

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs        -> (module PVS)
  | `Coq        -> (module COQ)
  | `Matita     -> (module MATITA)
  | `OpenTheory -> (module OPENTHEORY)
  | `Lean       -> (module LEAN)
  | `Sttfa    -> (module STTFA)
