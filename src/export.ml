type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Dksttfa]

module type E =
sig
  val extension: string
  val print_ast : out_channel -> string -> Ast.ast -> unit
end

module type BDD =
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

module PVS : BDD =
struct
  let extension = "pvs"
  let print_ast = Pvs.print_ast
  let print_bdd = Pvs.print_bdd
end

module COQ : BDD =
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

module MATITA : BDD =
struct
  let extension = "ma"
  let print_ast = Matita.print_ast
  let print_bdd = Matita.print_bdd
end

module OPENTHEORY : BDD =
struct
  let extension = "art"
  let print_ast = OpenTheory.print_ast
  let print_bdd = OpenTheory.print_bdd
end

(*
module LEAN : E =
struct
  let extension = "lean"
  let print_ast = Lean.print_ast
end
*)
module LEAN : BDD =
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
  | `OpenTheory -> failwith "todo"
  | `Lean       -> failwith "todo"
  | `Dksttfa    -> failwith "todo"
