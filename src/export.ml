type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Dksttfa]

module type E =
sig
  val extension: string
  val print_ast : Format.formatter -> string -> Ast.ast -> unit
  val print_bdd : Ast.ast -> unit
end
(*
module PVS : E =
struct
  let extension = "pvs"
  let print_ast = Pvs.print_ast
end
*)

module PVS : E =
struct
  let extension = "pvs"
  let print_ast = Pvs.print_ast
  let print_bdd = Pvs.print_bdd
end

module COQ : E =
struct
  let extension = "v"
  let print_ast = Coq.print_ast
  let print_bdd = Coq.print_bdd
end

(*
module MATITA : E =
struct
  let extension = "ma"
  let print_ast = Matita.print_ast
end
*)

module MATITA : E =
struct
  let extension = "ma"
  let print_ast = Matita.print_ast
  let print_bdd = Matita.print_bdd
end

module OPENTHEORY : E =
struct
  let extension = "art"
  let print_ast = Opentheory.print_ast
  let print_bdd = failwith "todo"
end

module LEAN : E =
struct
  let extension = "lean"
  let print_ast = Lean.print_ast
  let print_bdd = Lean.print_bdd
end


let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs        -> failwith "todo"
  | `Coq        -> failwith "todo"
  | `Matita     -> failwith "todo"
  | `OpenTheory -> (module OPENTHEORY)
  | `Lean       -> failwith "todo"
  | `Dksttfa    -> failwith "todo"
