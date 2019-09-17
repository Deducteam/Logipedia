(** Export to json files. *)

open Extras
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
    assert (stk = []); (* In β normal form *)
    let bound = B.string_of_ident id in
    let annotation = Option.bind ppt_of_dkterm annot in
    Jt.Ppterm.Binder { b_symb = "λ" ; bound ; annotation
                     ; body = ppt_of_dkterm t }
  | T.Pi(_,id,t,u) ->
    assert (stk = []); (* In β normal form *)
    let annotation = Some(ppt_of_dkterm t) in
    let body = ppt_of_dkterm u in
    let bound = B.string_of_ident id in
    Jt.Ppterm.Binder { b_symb = "Π" ; bound ; annotation ; body }

let item_of_entry : Entry.entry -> Jt.item = function
  | Entry.Decl(_,id,_,t)  ->
    let ppt = ppt_of_dkterm t in
    { name = B.string_of_ident id
    ; taxonomy = Uri.TxDef (* wrong *)
    ; term = ppt
    ; body = ppt
    ; deps = []
    ; theory = []
    ; exp = [] }
  | Entry.Def(_,_,_,_,_)  -> failwith "not implemented"
  | _                     -> failwith "not implemented"

let export_document : Jt.document -> unit = fun _ -> assert false
