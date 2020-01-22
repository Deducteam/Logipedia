module B = Kernel.Basic
module D = Api.Dep
module E = Parsing.Entry
module T = Kernel.Term
module U = Core.Uri

type tx =
  | TxAxm (** Axiom *)
  | TxDef (** Definition *)
  | TxCst (** Constant *)
  | TxThm (** Theorem *)

type item = Sttfa__Ast.item

exception IllTaxon
(** Exception raised when reading an ill formed taxon. *)

let theory = "sttfa"

let encoding = [B.mk_mident "sttfa"]

let tx_of_def = fun t _ ->
  match t with
  | Some(T.App(Const(_,name),_,_)) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident theory) ->
    TxDef
  | Some(App(Const(_,name),_,_)) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident theory) ->
    TxThm
  | _ -> TxDef

let tx_of_decl = function
  | T.App(Const(_,name),_,_) when
      (B.id name = B.mk_ident "etap" && B.md name = B.mk_mident theory) ->
    TxCst
  | App(Const(_,name),_,_) when
      (B.id name = B.mk_ident "eps" && B.md name = B.mk_mident theory) ->
    TxAxm
  | _ -> TxCst

let string_of_tx ?(short=false) tx =
  match tx with
  | TxAxm -> if short then "axm" else "axiom"
  | TxDef -> if short then "def" else "definition"
  | TxCst -> if short then "cst" else "constant"
  | TxThm -> if short then "thm" else "theorem"

let tx_of_string s =
  if      s = "axiom"      || s = "axm" then TxAxm
  else if s = "definition" || s = "def" then TxDef
  else if s = "constant"   || s = "cst" then TxCst
  else if s = "theorem"    || s = "thm" then TxThm
  else raise IllTaxon

let is_axiomatic : tx -> bool = (=) TxAxm

let fields_of_def tx teo te = match tx with
  | TxThm ->
    (* Don't export the proof which is in [te] *)
    begin match teo with
      | Some(t) -> (t, None)
      | None    -> assert false
      (* A theorem always have a statement and a proof. *)
    end
  | TxDef -> te, teo
  | _     -> assert false

let label = function
  | TxCst -> ("type", None)
  | TxAxm -> ("statement", None)
  | TxDef -> ("body", Some("type_annotation"))
  | TxThm -> ("statement", None)

let item_of_entry mident entry = Sttfa__Compile.compile_entry mident entry

let string_of_item item system =
  Sttfa.Export.export_to_system_as_string system item
