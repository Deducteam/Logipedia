(** Export to json files. *)

open Extras
module B = Kernel.Basic
module D = Api.Dep
module Ent = Parsers.Entry
module Env = Api.Env
module F = Format
module S = Kernel.Signature
module T = Kernel.Term
module U = Uri
module Jt = Json_types
module Tx = Taxonomy

(** Information collected in the current time. *)
type content =
  { ct_taxo : Tx.Sttfa.t NameMap.t
  (** Taxons of the file. *)
  ; ct_deps : (B.name list) NameMap.t
  (** Dependencies *)
  ; ct_thax : (B.name list) NameMap.t
  (** Axiomatic theories *) }

(** {2 Loading from other json files} *)

(** [find_taxon n] finds taxon of Dedukti name [n] across available
    json files. *)
let find_taxon : B.name -> Tx.Sttfa.t =
  (* Some memoization *)
  let taxons = NameHashtbl.create 0 in
  fun key ->
  try NameHashtbl.find taxons key
  with Not_found ->
    (* Parse the correct json file *)
    (* Output file must be in the same dir than other jsons *)
    let fname = B.md key |> B.string_of_mident in
    let fullpath = Filename.concat !(Jt.json_dir) (fname ^ ".json") in
    let doc = Yojson.Safe.from_file fullpath
              |> Jt.document_of_yojson
    in
    let f it =
      let uri = U.of_string it.Jt.name in
      let nm = U.name_of_uri uri in
      let tx = U.ext_of_uri uri |> Tx.Sttfa.of_string in
      NameHashtbl.add taxons nm tx
    in
    match doc with
    | Result.Error(s) ->
      failwith (Format.sprintf
                  "Error parsing file %s at line %s (as dependency)" fullpath s)
    | Result.Ok(doc) ->
      List.iter f doc;
      NameHashtbl.find taxons key

(** [find_deps m i e] computes the list of all direct down
    dependencies of a Dedukti entry [e] with name [m.i] as a list of
    Dedukti names. *)
let find_deps : B.mident -> Ent.entry -> B.name list = fun mid e ->
  let id = match e with
    | Ent.Decl(_,id,_,_)
    | Ent.Def(_,id,_,_,_) -> id
    | _                 -> assert false
  in
  D.compute_all_deps := true;
  D.make mid [e];
  let name = B.mk_name mid id in
  match D.get_data name with
  | exception D.Dep_error(D.NameNotFound(_)) -> []
  | d ->
    (* Remove some elements from dependencies and create a part of the uri. *)
    let f n = B.string_of_mident (B.md n) = Tx.Sttfa.theory in
    List.filter f (B.NameSet.elements D.(d.down))

(** {2 Loading from currently parsed file} *)

(** [find_taxon ct n] searches for taxon of [n] in the current content [ct] and
    hands over {!val:find_taxon} if it is not found. *)
let find_taxon : content -> B.name -> Tx.Sttfa.t = fun ct nm ->
  try NameMap.find nm ct.ct_taxo
  with Not_found -> find_taxon nm

(** [ppt_of_dkterm md tx te] converts Dedukti term [te] from Dedukti
    module [md] into a JSON ppterm of taxonomy [tx]. *)
let rec ppt_of_dkterm : B.mident -> content -> T.term -> Jt.Ppterm.t =
  fun md acc t ->
  ppt_of_dkterm_args md acc t []

(** [ppt_of_dkterm_args md tx te stk] converts Dedukti term [te] from
    module [md] applied to stack of arguments [stk].  [tx] is the taxon of
    [te]. *)
and ppt_of_dkterm_args : B.mident -> content -> T.term -> T.term list -> Jt.Ppterm.t =
  fun md acc t stk ->
  let ppt_of_dkterm = ppt_of_dkterm md acc in
  let ppt_of_dkterm_args = ppt_of_dkterm_args md acc in
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
      let c_tx = find_taxon acc name in
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

(** {2 Processor} *)

module DocumentBuilder = struct
  type t = Jt.document

  let _acc : Jt.document ref = ref []

  let _ct = ref { ct_taxo = NameMap.empty
                ; ct_deps = NameMap.empty
                ; ct_thax = NameMap.empty }

  let get_data () = !_acc

  let handle_entry: Env.t -> Ent.entry -> unit = fun env ent ->
    let mdl = Env.get_name env in
    match ent with
    | Ent.Decl(_,id,_,_)
    | Ent.Def(_,id,_,_,_) ->
      let inm = B.mk_name mdl id in
      let deps = find_deps mdl ent in
      _ct := {!_ct with ct_deps = NameMap.add inm deps !_ct.ct_deps};
      let deps =
        let fill n =
          U.of_dkname n Tx.Sttfa.theory
            (Tx.Sttfa.to_string ~short:true (find_taxon !_ct n))
        in
        List.map fill deps
      in
      let tx =
        match ent with
        | Ent.Decl(_,_,_,t)  -> Tx.Sttfa.of_decl t
        | Ent.Def(_,_,_,_,t) -> Tx.Sttfa.of_def t
        | _                -> assert false
      in
      let label = Tx.Sttfa.label tx in
      _ct := {!_ct with ct_taxo = NameMap.add inm tx !_ct.ct_taxo };
      let uri = U.of_dkname (B.mk_name mdl id) Tx.Sttfa.theory
          (Tx.Sttfa.to_string ~short:true tx) |> U.to_string
      in
      begin match ent with
        | Ent.Decl(_,_,_,t) ->
          let ppt_term =  ppt_of_dkterm mdl !_ct t in
          _acc := { Jt.name = uri
                  ; taxonomy = Tx.Sttfa.to_string tx
                  ; term = ppt_term
                  ; term_opt = None
                  ; label
                  ; deps = List.map U.to_string deps
                  ; theory = []
                  ; exp = [] } :: !_acc
        | Ent.Def(_,_,_,teo,te)  ->
          let ppt_term = ppt_of_dkterm mdl !_ct te in
          let ppt_term_opt = Option.map (ppt_of_dkterm mdl !_ct) teo in
          _acc := { name = uri
                  ; taxonomy = Tx.Sttfa.to_string tx
                  ; term = ppt_term
                  ; term_opt = ppt_term_opt
                  ; label
                  ; deps = List.map U.to_string deps
                  ; theory = []
                  ; exp = [] } :: !_acc
        | _               -> ()
      end
    | _ -> assert false
end

let print_document : Format.formatter -> Jt.document -> unit = fun fmt doc ->
  Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
