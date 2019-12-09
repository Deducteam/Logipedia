module B = Kernel.Basic
module D = Api.Dep
module E = Parsing.Entry
module T = Kernel.Term
module U = Core.Uri
module type S = sig
  type tx
  type item
  exception IllTaxon
  val theory : string
  val tx_of_def : T.term option -> T.term -> tx
  val tx_of_decl : T.term -> tx
  val string_of_tx : ?short:bool -> tx -> string
  val tx_of_string : string -> tx
  val is_axiomatic : tx -> bool
  val fields_of_def : tx -> 'a option -> 'a -> 'a * 'a option
  val label : tx -> string * string option
  val item_of_entry : string -> E.entry -> item
  val string_of_item : item -> Core.Systems.system -> string
end

module Dummy : S =
struct
  type tx = unit
  type item = unit
  let theory = "dummy"
  exception IllTaxon
  let tx_of_def _ _ = ()
  let tx_of_decl _ = ()
  let string_of_tx ?short:_ _ = "dummy"
  let tx_of_string _ = ()
  let is_axiomatic _ = false
  let fields_of_def _ _ t = t,None
  let label _ = "dummy",None
  let item_of_entry _ _ = ()
  let string_of_item _ _ = "dummy"
end
