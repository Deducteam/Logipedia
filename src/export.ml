type system = [`Coq | `Matita | `Pvs | `Latex | `Csv | `OpenTheory ]

module type E =
sig
  val extension: string
  val print_ast : out_channel -> string -> Ast.ast -> unit
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

module CSV : E =
struct
  let extension = "art"
  let print_ast = Csv.print_ast
end

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs        -> (module PVS)
  | `Coq        -> (module COQ)
  | `Matita     -> (module MATITA)
  | `OpenTheory -> (module OPENTHEORY)
  | `Csv        -> (module CSV)
  | _ -> failwith "not implemented yet"
