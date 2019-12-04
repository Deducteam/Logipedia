(** Export to json files. *)

open Extras
module B = Kernel.Basic
module D = Api.Dep
module E = Parsing.Entry
module F = Format
module M = Middleware
module S = Kernel.Signature
module T = Kernel.Term
module U = Uri
module Jt = Json_types
module Sy = Systems

(** Basename of the processed file, that is, dedukti module. *)
let basename : string ref = ref ""

module type S = functor (M: Middleware.S) ->
sig
  val doc_of_entries : B.mident -> E.entry list -> Jt.document
  val print_document : Format.formatter -> Jt.document -> unit
end

module Make(M:Middleware.S) =
struct

  (** Information collected in the current time. *)
  type content =
    { ct_taxo : M.tx NameMap.t
    (** Taxons of the file. *)
    ; ct_deps : (B.name list) NameMap.t
    (** Dependencies *)
    ; ct_thax : (B.name list) NameMap.t
    (** Axiomatic theories *)}

  (** {2 Loading from other json files} *)

  (** [find_taxon n] finds taxon of Dedukti name [n] across available
      json files. *)
  let find_taxon : B.name -> M.tx =
    (* Some memoization *)
    let taxons = NameHashtbl.create 0 in
    fun key ->
      try NameHashtbl.find taxons key
      with Not_found ->
        (* Parse the correct json file *)
        (* Output file must be in the same dir than other jsons *)
        let fname = B.md key |> B.string_of_mident in
        let doc =
          try
            let fullpath = Filename.concat !(Jt.json_dir) (fname ^ ".json") in
            Yojson.Safe.from_file fullpath |> Jt.document_of_yojson
          with Sys_error(_) ->
            (* If the file has not been found, it is probably a theory file,
               lying in the [theory] subdirectory. *)
            let fullpath = String.concat Filename.dir_sep
                [ !Jt.json_dir ; Jt.json_thy_dir ; (fname ^ ".json") ] in
            Yojson.Safe.from_file fullpath |> Jt.document_of_yojson
        in
        let f it =
          let uri = U.of_string it.Jt.name in
          let nm = U.name_of_uri uri in
          let tx = U.ext_of_uri uri |> M.tx_of_string in
          NameHashtbl.add taxons nm tx
        in
        match doc with
        | Result.Error(s) ->
          failwith
            (Format.sprintf "Error parsing file at line %s (as dependency)" s)
        | Result.Ok(doc) ->
          List.iter f doc;
          NameHashtbl.find taxons key

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
      let f n = B.string_of_mident (B.md n) <> M.theory in
      List.filter f (D.NameSet.elements D.(d.down))

  (** {2 Loading from currently parsed file} *)

  (** [find_taxon ct n] searches for taxon of [n] in the current content [ct]
      and hands over {!val:find_taxon} if it is not found. *)
  let find_taxon : content -> B.name -> M.tx = fun ct nm ->
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
  and ppt_of_dkterm_args : B.mident -> content -> T.term -> T.term list
    -> Jt.Ppterm.t =
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
        let tx = M.string_of_tx ~short:true c_tx in
        U.of_dkname (B.mk_name cmd cid) M.theory tx |> U.to_string
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

  (** {2 Exposed functions} *)

  let doc_of_entries : B.mident -> E.entry list -> Jt.item list =
    fun mdl entries ->
    let init = { ct_taxo = NameMap.empty
               ; ct_deps = NameMap.empty
               ; ct_thax = NameMap.empty }
    in
    let rec loop : content -> E.entry list -> Jt.item list = fun acc ens ->
      match ens with
      | []      -> []
      | e :: tl ->
        match e with
        | E.Decl(_,id,_,_)
        | E.Def(_,id,_,_,_) ->
          let inm = B.mk_name mdl id in
          let deps = find_deps mdl e in
          let acc =
            let ct_deps = NameMap.add inm deps acc.ct_deps in
            { acc with ct_deps }
          in
          let deps =
            let fill n =
              U.of_dkname n M.theory
                (M.string_of_tx ~short:true (find_taxon acc n))
            in
            List.map fill deps
          in
          let tx =
            match e with
            | E.Decl(_,_,_,t)  -> M.tx_of_decl t
            | E.Def(_,_,_,t,u) -> M.tx_of_def t u
            | _                -> assert false
          in
          let label = M.label tx in
          let acc = { acc with ct_taxo = NameMap.add inm tx acc.ct_taxo } in
          let uri = U.of_dkname (B.mk_name mdl id) M.theory
              (M.string_of_tx ~short:true tx) |> U.to_string
          in
          let art2exp (sys, pth) =
            let ext = List.assoc sys Sy.sys_ext in
            let file = Filename.concat pth (!basename ^ "." ^ ext) in
            { Jt.system = Sy.string_of_system sys
            ; file
            ; etype = Some (M.string_of_item e sys) }
          in
          let exp = List.map art2exp !Sy.artefact_path in
          begin match e with
            | E.Decl(_,_,_,t) ->
              let ppt_term =  ppt_of_dkterm mdl acc t in
              { name = uri
              ; taxonomy = M.string_of_tx tx
              ; term = ppt_term
              ; term_opt = None
              ; label
              ; deps = List.map U.to_string deps
              ; theory = []
              ; exp } :: (loop acc tl)
            | E.Def(_,_,_,teo,te)  ->
              (* We use lazy to remap the computation, and avoid computing the
                 ppterm then discard it. *)
              let lppt = lazy (ppt_of_dkterm mdl acc te) in
              let lppto = Option.map
                  (fun t -> lazy (ppt_of_dkterm mdl acc t)) teo
              in
              let (lazy term, term_opt) = M.fields_of_def tx lppto lppt in
              let term_opt = Option.map Lazy.force term_opt in
              { name = uri
              ; taxonomy = M.string_of_tx tx
              ; term
              ; term_opt
              ; label
              ; deps = List.map U.to_string deps
              ; theory = []
              ; exp } :: (loop acc tl)
            | _                     -> loop acc tl
          end
        | _ -> loop acc tl
    in
    loop init entries

  let print_document : Format.formatter -> Jt.document -> unit = fun fmt doc ->
    Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
end
