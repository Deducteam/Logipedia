(** Export to json files. *)

open Extras
module F = Format
module Jt = Json_types
module B = Basic
module T = Term
module U = Uri
module S = Signature

(** The theory (or logic) used. *)
let _th = (`Sttfa)

(** [ppt_of_dkterm md tx te] converts Dedukti term [te] from Dedukti
    module [md] into a JSON ppterm of taxonomy [tx]. *)
let rec ppt_of_dkterm : B.mident -> U.taxon -> T.term -> Jt.Ppterm.t =
  fun md tx t ->
  ppt_of_dkterm_args md tx t []

(** [ppt_of_dkterm_args md tx te stk] converts Dedukti term [te] from
    module [md] applied to stack of arguments [stk].  [tx] is the taxon of
    [te]. *)
and ppt_of_dkterm_args : B.mident -> U.taxon -> T.term -> T.term list ->
  Jt.Ppterm.t =
  fun md tx t stk ->
  let ppt_of_dkterm = ppt_of_dkterm md tx in
  let ppt_of_dkterm_args = ppt_of_dkterm_args md tx in
  match t with
  | T.Kind -> Jt.Ppterm.Const { c_symb = "Kind" ; c_args = [] }
  | T.Type(_) -> Jt.Ppterm.Const { c_symb = "Type" ; c_args = [] }
  | T.DB(_,id,_) ->
    let v_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Var { v_symb = B.string_of_ident id ; v_args}
  | T.Const(_,name) ->
    let c_args = List.map ppt_of_dkterm stk in
    let c_symb =
      let mid = B.md name in
      let id = B.id name in
      U.uri_of_dkid mid id _th tx |> U.to_string
    in
    Jt.Ppterm.Const { c_symb ; c_args }
  | T.App(t,u,vs) -> ppt_of_dkterm_args t (u :: vs @ stk)
  | T.Lam(_,id,annot,t) ->
    let bound = B.string_of_ident id in
    let annotation = Option.map ppt_of_dkterm annot in
    let b_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Binder { b_symb = "λ" ; bound ; annotation
                     ; body = ppt_of_dkterm t ; b_args }
  | T.Pi(_,id,t,u) ->
    let annotation = Some(ppt_of_dkterm t) in
    let body = ppt_of_dkterm u in
    let bound = B.string_of_ident id in
    let b_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Binder { b_symb = "Π" ; bound ; annotation ; body ; b_args }

let item_of_entry : B.mident -> Entry.entry -> Jt.item option = fun md en ->
  match en with
  | Entry.Decl(_,id,static,t) ->
    let tx = if static = S.Static then U.TxCst else U.TxAxm in
    let uri = U.uri_of_dkid md id _th U.TxDef |> U.to_string in
    Some { name = uri
         ; taxonomy = tx
         ; term = None
         ; body = ppt_of_dkterm md tx t
         ; deps = []
         ; theory = []
         ; exp = [] }
  | Entry.Def(_,id,opacity,teo,te)  ->
    let tx = if opacity then U.TxDef else U.TxThm in
    let uri = U.uri_of_dkid md id _th U.TxDef |> U.to_string in
    Some { name = uri
         ; taxonomy = tx
         ; term = Option.map (ppt_of_dkterm md tx) teo
         ; body = ppt_of_dkterm md tx te
         ; deps = []
         ; theory = []
         ; exp = [] }
  | _                     -> None

let print_document : Format.formatter -> Jt.document -> unit = fun fmt doc ->
  Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
