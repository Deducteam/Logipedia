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

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs -> (module PVS)
  | _ -> failwith "not implemented yet"
