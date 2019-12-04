module B = Kernel.Basic
module D = Api.Dep
module T = Kernel.Term
module U = Uri

module Cic : Middleware.S =
struct
  type tx =
    | TxAxiom          (** Axiom *)
    | TxDefinition     (** Definition *)
    | TxConstant       (** Constant *)
    | TxTheorem        (** Theorem *)
    | TxIndType        (** Inductive type *)
    | TxIndConstructor (** Inductive constructor *)
    | TxIndDestructor  (** Inductive constructor *)

  exception IllTaxon
  (** Exception raised when reading an ill formed taxon. *)

  let theory = "cic"

  let tx_of_def _ _ = TxDefinition

  let tx_of_decl _ = TxAxiom

  let string_of_tx ?(short=false) = function
    | TxAxiom          -> if short then "axm" else "axiom"
    | TxDefinition     -> if short then "def" else "definition"
    | TxConstant       -> if short then "cst" else "constant"
    | TxTheorem        -> if short then "thm" else "theorem"
    | TxIndType        -> if short then "ind" else "inductive_type"
    | TxIndConstructor -> if short then "cns" else "inductive_constructor"
    | TxIndDestructor  -> if short then "dst" else "inductive_destructor"

  let tx_of_string = function
    | "axiom"                 | "axm" -> TxAxiom
    | "definition"            | "def" -> TxDefinition
    | "constant"              | "cst" -> TxConstant
    | "theorem"               | "thm" -> TxTheorem
    | "inductive_type"        | "ind" -> TxIndType
    | "inductive_constructor" | "cns" -> TxIndConstructor
    | "inductive_destructor"  | "dst" -> TxIndDestructor
    | _ -> raise IllTaxon

  let is_axiomatic = function
    | TxAxiom | TxIndType | TxIndConstructor | TxIndDestructor -> true
    | _ -> false

  let fields_of_def tx teo te = match tx with
    | TxIndType | TxIndConstructor | TxIndDestructor | TxTheorem ->
      ( match teo with Some t -> (t, None) | None -> assert false)
    | _ -> (te, teo)

  let label = function
    (* Dedukti declarations: fst is the type  / snd is None *)
    | TxAxiom          -> (       "statement", None)
    | TxConstant       -> (            "type", None)
    (* Dedukti definitions: fst is body  / snd is the type *)
    | TxDefinition     -> (            "body", Some "type_annotation")
    (* Dedukti "special" definitions: fst is type  / snd is None *)
    | TxTheorem        -> (       "statement", None)
    | TxIndType        -> (  "inductive_type", None)
    | TxIndConstructor -> ("constructor_type", None)
    | TxIndDestructor  -> (      "match_type", None)

  (** TODO: Define cic items, and how to print them on the website depending on the export system *)
  let string_of_item  _ _ = ""
end
