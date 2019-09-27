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

(** Collection of the taxons of the currently built json file. *)
let taxons : Tx.Sttfa.t B.NameHashtbl.t = B.NameHashtbl.create 0

(** [find_taxon n] first searches for the taxon of Dk identifier [n] into the
    {!val:taxons} collection, and if not found, it will search among
    existing json files via the {!val:Tx.find_taxon} function. *)
let find_taxon : B.name -> Tx.Sttfa.t = fun n ->
  try B.NameHashtbl.find taxons n
  with Not_found -> Tx.find_taxon n

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
      let c_tx = find_taxon name in
      let tx = Tx.Sttfa.to_string ~short:true c_tx in
      U.of_dkname (B.mk_name cmd cid) Tx.Sttfa.theory tx |> U.to_string
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
    Dedukti names. *)
let find_deps : B.mident -> E.entry -> B.name list = fun mid e ->
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
    let f n = B.string_of_mident (B.md n) = Tx.Sttfa.theory in
    List.filter f (D.NameSet.elements D.(d.down))

let doc_of_entries : B.mident -> E.entry list -> Jt.item list =
  fun mdl entries ->
  let rec loop : E.entry list -> Jt.item list = fun ens ->
    match ens with
    | []      -> []
    | e :: tl ->
      match e with
      | E.Decl(_,id,_,_)
      | E.Def(_,id,_,_,_) ->
        let inm = B.mk_name mdl id in
        let deps =
          let d = find_deps mdl e in
          let fill n =
            U.of_dkname n Tx.Sttfa.theory
              (Tx.Sttfa.to_string ~short:true (find_taxon n))
          in
          List.map fill d
        in
        begin match e with
          | E.Decl(_,id,_,t) ->
            let tx = Tx.Sttfa.of_decl t in
            let uri = U.of_dkname (B.mk_name mdl id) Tx.Sttfa.theory
                (Tx.Sttfa.to_string ~short:true tx) |> U.to_string
            in
            let ppt_body =  ppt_of_dkterm mdl t in
            B.NameHashtbl.add taxons inm tx;
            { name = uri
            ; taxonomy = Tx.Sttfa.to_string tx
            ; term = None
            ; body = ppt_body
            ; deps = List.map U.to_string deps
            ; theory = []
            ; exp = [] } :: (loop tl)
          | E.Def(_,id,_,teo,te)  ->
            let tx = Tx.Sttfa.of_def te in
            let inm = B.mk_name mdl id in
            let uri = U.of_dkname (B.mk_name mdl id) Tx.Sttfa.theory
                (Tx.Sttfa.to_string ~short:true tx) |> U.to_string
            in
            B.NameHashtbl.add taxons inm tx;
            let ppt_body = ppt_of_dkterm mdl te in
            let ppt_term_opt = Option.map (ppt_of_dkterm mdl) teo in
            { name = uri
            ; taxonomy = Tx.Sttfa.to_string tx
            ; term = ppt_term_opt
            ; body = ppt_body
            ; deps = List.map U.to_string deps
            ; theory = []
            ; exp = [] } :: (loop tl)
          | _                     -> loop tl
        end
      | _ -> loop tl
  in
  loop entries

let print_document : Format.formatter -> Jt.document -> unit = fun fmt doc ->
  Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
