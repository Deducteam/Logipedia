type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Sttfa]

module type E =
sig
    val system    : system
    val extension : string
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
(*
module type JSON =
sig
  val print_json : Ast.ast -> unit
end

module MAKE_JSON(M:E) : JSON =
struct
  let theory_id =
    match M.system with
    | `Coq -> 1
    | `Matita -> 2
    | `Pvs -> 3
    | `OpenTheory -> 4
    | `Lean -> 5
    | `Sttfa -> 6

  let string_of_theory =
    match M.system with
    | `Coq -> "coq"
    | `Matita -> "matita"
    | `Pvs -> "pvs"
    | `OpenTheory -> "opentheory"
    | `Lean -> "lean"
    | `Sttfa -> "sttfa"

  let empty = Bson.empty

  let add_parameter k v cont =
    Bson.add_element k v cont

  let prefix = "json"
  let file md id =
    Format.asprintf "%s/%s_%a_%a" prefix string_of_theory Pp.print_mident md Pp.print_ident id

  let keyword_of_item item = failwith "todo"
  let statement_of_item item = failwith "todo"
  let type_of_item item = failwith "todo"
  let body_of_item item = failwith "todo"
  let print_json ast =
    Bson.to_simple_json
end
*)
let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Pvs        -> (module PVS)
  | `Coq        -> (module COQ)
  | `Matita     -> (module MATITA)
  | `OpenTheory -> (module OPENTHEORY)
  | `Lean       -> (module LEAN)
  | `Sttfa    -> (module STTFA)
