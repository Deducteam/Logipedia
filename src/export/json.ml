(** Export to json files. *)

open Extras
module F = Format
module Jt = Json_types
module B = Basic
module T = Term

let rec ppt_of_dkterm : T.term -> Jt.Ppterm.t = fun t ->
  ppt_of_dkterm_args t []

and ppt_of_dkterm_args : T.term -> T.term list -> Jt.Ppterm.t =
  fun t stk ->
  match t with
  | T.Kind -> Jt.Ppterm.Const { c_symb = ["Kind"] ; c_args = [] }
  | T.Type(_) -> Jt.Ppterm.Const { c_symb = ["Type"] ; c_args = [] }
  | T.DB(_,id,_) ->
    let v_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Var { v_symb = Basic.string_of_ident id ; v_args}
  | T.Const(_,name) ->
    let c_args = List.map ppt_of_dkterm stk in
    let c_symb = [B.id name |> B.string_of_ident] in
    Jt.Ppterm.Const { c_symb ; c_args }
  | T.App(t,u,vs) -> ppt_of_dkterm_args t (u :: vs @ stk)
  | T.Lam(_,id,annot,t) ->
    let bound = B.string_of_ident id in
    let annotation = Option.bind ppt_of_dkterm annot in
    let b_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Binder { b_symb = "λ" ; bound ; annotation
                     ; body = ppt_of_dkterm t ; b_args }
  | T.Pi(_,id,t,u) ->
    let annotation = Some(ppt_of_dkterm t) in
    let body = ppt_of_dkterm u in
    let bound = B.string_of_ident id in
    let b_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Binder { b_symb = "Π" ; bound ; annotation ; body ; b_args }

let item_of_entry : Entry.entry -> Jt.item option = function
  | Entry.Decl(_,id,_,t)  ->
    let ppt = ppt_of_dkterm t in
    Some { name = B.string_of_ident id
         ; taxonomy = Uri.TxDef (* wrong *)
         ; term = Some(ppt)
         ; body = ppt
         ; deps = []
         ; theory = []
         ; exp = [] }
  | Entry.Def(_,id,_,teo,te)  ->
    Some { name = B.string_of_ident id
         ; taxonomy = Uri.TxDef
         ; term = Option.bind ppt_of_dkterm teo
         ; body = ppt_of_dkterm te
         ; deps = []
         ; theory = []
         ; exp = [] }
  | _                     -> None

let print_document : Format.formatter -> Jt.document -> unit = fun fmt doc ->
  Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
