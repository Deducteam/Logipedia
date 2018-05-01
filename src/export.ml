type system = [`Coq | `Matita | `Pvs | `Latex | `Ascii | `OpenTheory ]

module type E =
sig
  val extension: string
  val print_ast : out_channel -> string -> Ast.ast -> unit
end

module PVS : E =
struct
  let extension = "pvs"
  let print_ast = Pvs.print_ast_pvs
end

module COQ : E =
struct
  let extension = "v"
  let print_ast = Coq.print_ast_coq
end

module MATITA : E =
struct
  let extension = "ma"
  let print_ast = Matita.print_ast_coq
end

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs    -> (module PVS)
  | `Coq    -> (module COQ)
  | `Matita -> (module MATITA)
  | _ -> failwith "not implemented yet"
