module A = Ast
module B = Kernel.Basic
module D = Deps
module E = Parsing.Entry
module Denv = Api.Env.Default
module P = Parsing.Parser
open Systems

module type E =
sig
  val system         : Systems.system
  val extension      : string
  val print_ast      : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit
  val string_of_item : Ast.item -> string
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

let of_system : system -> (module E) = fun sys ->
  match sys with
  | `Coq        -> (module COQ)
  | `Matita     -> (module MATITA)
  | `OpenTheory -> (module OPENTHEORY)
  | `Lean       -> (module LEAN)
  | `Pvs        -> failwith "Dedicated binary"

(* FIXME: this function is sttfa specific *)
(** [mk_ast md es] creates the STTfa ast of entries [es] from dedukti module
    [md] *)
let mk_ast : B.mident -> E.entry list -> A.ast = fun md entries ->
  let items = List.map (Compile.compile_entry md) entries in
  let fold_entry_dep dep e = D.QSet.union dep
      (Deps.dep_of_entry [Sttfadk.sttfa_module;md] e) in
  let dep = List.fold_left fold_entry_dep D.QSet.empty entries in
  { Ast.md = B.string_of_mident md; Ast.dep; items }

(* FIXME put in a sttfa module *)
let export_system : (module E) -> string -> Format.formatter -> unit =
  fun (module M:E) infile outfmt ->
  let md = Denv.init infile in
  let input = open_in infile in
  let entries = P.Parse_channel.parse md input in
  close_in input;
  let sttfa_ast = mk_ast md entries in
  M.print_ast outfmt sttfa_ast
