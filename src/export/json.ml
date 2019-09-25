(** Export to json files. *)

open Extras
module B = Basic
module D = Dep
module E = Entry
module F = Format
module S = Signature
module T = Term
module U = Uri
module Jt = Json_types
module Tx = Taxonomy

(** [ppt_of_dkterm md tx te] converts Dedukti term [te] from Dedukti
    module [md] into a JSON ppterm of taxonomy [tx]. *)
let rec ppt_of_dkterm : B.mident -> T.term -> Jt.Ppterm.t =
  fun md t ->
  ppt_of_dkterm_args md t []

(** [ppt_of_dkterm_args md tx te stk] converts Dedukti term [te] from
    module [md] applied to stack of arguments [stk].  [tx] is the taxon of
    [te]. *)
and ppt_of_dkterm_args : B.mident -> T.term -> T.term list -> Jt.Ppterm.t =
  fun md t stk ->
  let ppt_of_dkterm = ppt_of_dkterm md in
  let ppt_of_dkterm_args = ppt_of_dkterm_args md in
  match t with
  | T.Kind -> Jt.Ppterm.Const { c_symb = "Kind" ; c_args = [] }
  | T.Type(_) -> Jt.Ppterm.Const { c_symb = "Type" ; c_args = [] }
  | T.DB(_,id,_) ->
    let v_args = List.map ppt_of_dkterm stk in
    Jt.Ppterm.Var { v_symb = B.string_of_ident id ; v_args}
  | T.Const(_,name) ->
    let c_args = List.map ppt_of_dkterm stk in
    let c_symb =
      let cmd = B.md name in
      let cid = B.id name in
      let c_tx = Tx.find_taxon name in
      let tx = Tx.Sttfa.to_string ~short:true c_tx in
      U.uri_of_dkid cmd cid Tx.Sttfa.theory tx |> U.to_string
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

(** [find_deps m i e] computes the list of all direct down
    dependencies of a Dedukti entry [e] with name [m.i] as a list of
    uris. *)
let find_deps : B.mident -> E.entry -> U.t list = fun mid e ->
  let id = match e with
    | E.Decl(_,id,_,_)
    | E.Def(_,id,_,_,_) -> id
    | _                 -> assert false
  in
  D.compute_ideps := true; (* Compute dependencies of items *)
  D.make mid [e];
  let name = B.mk_name mid id in
  match D.get_data name with
  | exception D.Dep_error(D.NameNotFound(_)) -> []
  | d ->
    (* Remove some elements from dependencies and create a part of the uri. *)
    let f n =
      if B.string_of_mident (B.md n) = Tx.Sttfa.theory then None else
      let tx = Tx.find_taxon n |> Tx.Sttfa.to_string ~short:true in
      let uri = U.uri_of_dkid (B.md n) (B.id n) Tx.Sttfa.theory tx in
      Some(uri)
    in
    List.filter_map f (D.NameSet.elements D.(d.down))

let item_of_entry : B.mident -> E.entry -> Jt.item option = fun md en ->
  match en with
  | Entry.Decl(_,id,_,t) ->
    let tx = Tx.Sttfa.of_decl t in
    let uri = U.uri_of_dkid md id Tx.Sttfa.theory
        (Tx.Sttfa.to_string ~short:true tx) |> U.to_string
    in
    let ppt_body =  ppt_of_dkterm md t in
    Some { name = uri
         ; taxonomy = Tx.Sttfa.to_string tx
         ; term = None
         ; body = ppt_body
         ; deps = List.map U.to_string (find_deps md en)
         ; theory = []
         ; exp = [] }
  | Entry.Def(_,id,_,teo,te)  ->
    let tx = Tx.Sttfa.of_def te in
    let uri = U.uri_of_dkid md id Tx.Sttfa.theory
        (Tx.Sttfa.to_string ~short:true tx) |> U.to_string
    in
    let ppt_body = ppt_of_dkterm md te in
    let ppt_term_opt = Option.map (ppt_of_dkterm md) teo in
    Some { name = uri
         ; taxonomy = Tx.Sttfa.to_string tx
         ; term = ppt_term_opt
         ; body = ppt_body
         ; deps = List.map U.to_string (find_deps md en)
         ; theory = []
         ; exp = [] }
  | _                     -> None

let print_document : Format.formatter -> Jt.document -> unit = fun fmt doc ->
  Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
