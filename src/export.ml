open Systems

module type Ei =
sig
  val system         : Systems.system
  val extension      : string
  val print_ast      : Api.Env.t -> Format.formatter -> ?mdeps:Ast.mdeps
                       -> Ast.ast -> unit
  val string_of_item : Api.Env.t -> Ast.item -> string
end

module PVS : Ei =
struct
  include Pvs
  let system    = `Pvs
  let extension = "pvs"
end

module COQ : Ei =
struct
  include Coq
  let system    = `Coq
  let extension = "v"
end

module MATITA : Ei =
struct
  include Matita
  let system    = `Matita
  let extension = "ma"
end

module OPENTHEORY : Ei =
struct
  include Opentheory
  let system    = `OpenTheory
  let extension = "art"
end

module LEAN : Ei =
struct
  include Lean
  let system    = `Lean
  let extension = "lean"
end

module type Eo =
sig
  include Ei
  val print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit
  val string_of_item : Ast.item -> string
end


let mk_exporter : Api.Env.t -> system -> (module Eo) = fun env sys ->
  let (module Exin : Ei) =
    match sys with
    | `Pvs        -> (module PVS)
    | `Coq        -> (module COQ)
    | `Matita     -> (module MATITA)
    | `OpenTheory -> (module OPENTHEORY)
    | `Lean       -> (module LEAN)
  in
  (module struct
    include Exin

    let print_ast = print_ast env
    let string_of_item = string_of_item env
  end)
