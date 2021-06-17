module B = Kernel.Basic
module D = Api.Dep
module E = Parsers.Entry
module R = Kernel.Rule
module T = Kernel.Term
module U = Core.Uri

type tx =
  | TxFun (** Definition *)
  | TxRule (** Rewrite rule *)
  | TxInd (** Inductive Type *)
  | TxCons (** Constructor of a type *)
  | TxProj (** Projection *)

type item = Parsers.Entry.entry

exception IllTaxon
(** Exception raised when reading an ill formed taxon. *)

let theory = "Agda"

let encoding = [B.mk_mident "Agda", B.mk_mident "univ"]

let right_most : T.term -> T.term = fun ty ->
  let rec right_most' = function
    | T.App(T.Const(_,cst),_,_::_::rig::[]) when
	(B.id cst = B.mk_ident "prod" && B.md cst = B.mk_mident theory) ->
       right_most' rig
    | T.App(T.Const(_,cst),_,rig::[]) when
	(B.id cst = B.mk_ident "qLevel" && B.md cst = B.mk_mident theory) ->
       right_most' rig
    | t -> t
  in
  match ty with
  | T.App(T.Const(_,cst),_,t::[]) when
	(B.id cst = B.mk_ident "Term" && B.md cst = B.mk_mident theory) ->
     right_most' t
  | _ -> assert false (* A type in the encoding should always start by "Agda.Term" *)

let is_univ : T.term -> bool = function
  | T.App(T.Const(_,cst),_,[]) ->
     B.id cst = B.mk_ident "code"
     && B.md cst = B.mk_mident theory
  | _ -> false

let contain_proj_prod : T.term -> bool = fun ty ->
  match right_most ty with
  | T.App(T.Const(_,cst),_,_::_::_::[]) ->
     B.id cst = B.mk_ident "proj_prod"
     && B.md cst = B.mk_mident theory
  | _ -> false

let tx_of_entry = function
  | E.Def(_)  -> assert false (* The encoding of Agda do not produce any definition *)
  | E.Decl(_,_,_,st,ty) ->
    let tx =
      match st with
      | Kernel.Signature.Static ->
        if is_univ (right_most ty) then TxInd else TxCons
      | Kernel.Signature.Definable(_) ->
        if contain_proj_prod ty then TxProj else TxFun
      | Kernel.Signature.Injective -> assert false
    in
    Some(tx)
  | E.Rules(_,r::[])  ->
     begin
       match r.R.pat with
       | R.Pattern(_,cst,_) when
	   (B.id cst = B.mk_ident "Term" && B.md cst = B.mk_mident theory) ->
	  None
       | R.Pattern(_,cst,_) when
	   (B.id cst = B.mk_ident "etaExpand" && B.md cst = B.mk_mident theory) ->
	  None
       | _ -> Some TxRule
     end
  | E.Rules(_,_)      -> assert false (* All rule should be on its own entry. *)
  | _                 -> None

let string_of_tx ?(short=false) = function
  | TxFun  -> if short then "fun"  else "function"
  | TxRule -> if short then "rule" else "rewrite rule"
  | TxInd  -> if short then "ind"  else "inductive type"
  | TxCons -> if short then "cons" else "constructor"
  | TxProj -> if short then "proj" else "projection"

let tx_of_string s =
  if      s = "function"       || s = "fun"  then TxFun
  else if s = "rewrite rule"   || s = "rule" then TxRule
  else if s = "inductive type" || s = "ind"  then TxInd
  else if s = "constructor"    || s = "cons" then TxCons
  else if s = "projection"     || s = "proj" then TxProj
  else raise IllTaxon

let is_axiomatic : tx -> bool = fun _ -> false

let fields_of_def _ teo te = (te,teo)

let label = function
  | TxRule -> ("lhs", Some "rhs")
  | TxFun  | TxInd  | TxCons | TxProj -> ("type", None)

let item_of_entry _ e = e

let string_of_item _ _ = "Not implemented feature"

let get_exporter = assert false
