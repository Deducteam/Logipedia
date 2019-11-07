module B = Kernel.Basic
module D = Api.Dep
module E = Parsing.Entry
module T = Kernel.Term
module U = Uri
module Jt = Json_types
module type S = sig
  type tx
  exception IllTaxon
  val theory : string
  val tx_of_def : T.term option -> T.term -> tx
  val tx_of_decl : T.term -> tx
  val string_of_tx : ?short:bool -> tx -> string
  val tx_of_string : string -> tx
  val is_axiomatic : tx -> bool
  val fields_of_def : tx -> 'a option -> 'a -> 'a * 'a option
  val label : tx -> string * string option
end

module Dummy : S =
struct
  type tx = unit
  let theory = "dummy"
  exception IllTaxon
  let tx_of_def _ _ = assert false
  let tx_of_decl _ = assert false
  let string_of_tx ?short:_ _ = assert false
  let tx_of_string _ = assert false
  let is_axiomatic _ = assert false
  let fields_of_def _ _ _ = assert false
  let label _ = assert false
end
