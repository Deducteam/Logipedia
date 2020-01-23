(** Export to json files. *)

open Core
open Console
open Extras
module B = Kernel.Basic
module D = Api.Dep
module E = Parsing.Entry
module T = Kernel.Term
module Jt = Json_types

let json_include : string ref = ref ""

let log_jscomp = new_logger "jscp"
let log_jscomp = log_jscomp.logger

module type S =
sig
  val doc_of_entries : B.mident -> E.entry list -> Jt.document
  val print_document : Jt.document pp
end

module Make (M:Middleware.S) : S =
struct

  (** Information collected in the current time. *)
  type content =
    { ct_taxo : M.tx NameMap.t
    (** Taxons of the file. *)
    ; ct_deps : (B.name list) NameMap.t
    (** Dependencies *)
    ; ct_thax : (B.name list) NameMap.t
    (** Axiomatic theories *) }

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
        let fullpath =
          let fname = B.md key |> B.string_of_mident in
          Filename.(!json_include </> (fname <.> "json"))
        in
        let doc =
          try
            Yojson.Safe.from_file fullpath |> Jt.document_of_yojson
          with Sys_error(_) ->
            (* If the file has not been found, it is probably a theory file,
               lying in the [theory] subdirectory. *)
            log_jscomp ~lvl:4 "reading json [%s]" fullpath;
            Yojson.Safe.from_file fullpath |> Jt.document_of_yojson
        in
        let f it =
          let uri = Uri.of_string it.Jt.name in
          let nm = Uri.name_of_uri uri in
          let tx = Uri.ext_of_uri uri |> M.tx_of_string in
          NameHashtbl.add taxons nm tx
        in
        match doc with
        | Result.Error(s) ->
          Console.exit_with "error parsing file [%s] line [%s] (as dep)"
            fullpath s
        | Result.Ok(doc)  ->
          List.iter f doc;
          NameHashtbl.find taxons key

  (** {2 Loading from currently parsed file} *)

  (** [find_taxon ct n] searches for taxon of [n] in the current content [ct]
      and hands over {!val:find_taxon} if it is not found. *)
  let find_taxon : content -> B.name -> M.tx = fun ct nm ->
    try NameMap.find nm ct.ct_taxo
    with Not_found -> find_taxon nm

  (** [ppt_of_dkterm md tx te] converts Dedukti term [te] from Dedukti
      module [md] into a JSON ppterm of taxonomy [tx]. *)
  let rec ppt_of_dkterm : DkTools.Mident.t -> content -> T.term -> Jt.Ppterm.t =
    fun md acc t ->
    ppt_of_dkterm_args md acc t []

  (** [ppt_of_dkterm_args md tx te stk] converts Dedukti term [te] from
      module [md] applied to stack of arguments [stk].  [tx] is the taxon of
      [te]. *)
  and ppt_of_dkterm_args : DkTools.Mident.t -> content -> T.term -> T.term list
    -> Jt.Ppterm.t =
    fun md acc t stk ->
    let ppt_of_dkterm = ppt_of_dkterm md acc in
    let ppt_of_dkterm_args = ppt_of_dkterm_args md acc in
    match t with
    | T.Kind -> Jt.Ppterm.Const { c_symb = "Kind" ; c_args = [] }
    | T.Type(_) -> Jt.Ppterm.Const { c_symb = "Type" ; c_args = [] }
    | T.DB(_,id,_) ->
      let v_args = List.map ppt_of_dkterm stk in
      Jt.Ppterm.Var { v_symb = B.string_of_ident id ; v_args }
    | T.Const(_,name) ->
      let c_args = List.map ppt_of_dkterm stk in
      let c_symb =
        let cmd = B.md name in
        let cid = B.id name in
        let c_tx = find_taxon acc name in
        let tx = M.string_of_tx ~short:true c_tx in
        Uri.of_dkname (B.mk_name cmd cid) M.theory tx |> Uri.to_string
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

  let doc_of_entries : DkTools.Mident.t -> E.entry list -> Jt.item list =
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
          log_jscomp ~lvl:6 "compiling [%a]" Api.Pp.Default.print_ident id;
          let inm = B.mk_name mdl id in
          let deps =
            let keep n = not @@ B.(List.mem_eq mident_eq (md n)) M.encoding in
            Deps.deps_of_entry mdl e |> List.filter keep
          in
          let acc =
            let ct_deps = NameMap.add inm deps acc.ct_deps in
            { acc with ct_deps }
          in
          let deps =
            let fill n =
              Uri.of_dkname n M.theory
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
          let uri = Uri.of_dkname (B.mk_name mdl id) M.theory
              (M.string_of_tx ~short:true tx) |> Uri.to_string
          in
          let art2exp (sys, pth) =
            let item =
              if List.mem_eq B.mident_eq mdl M.encoding then None else
              Some(M.item_of_entry mdl e)
            in
            let ext = List.assoc sys Systems.exts in
            let file =
              Filename.(pth </> (B.string_of_mident mdl <.> ext))
            in
            { Jt.system = Systems.to_string sys
            ; file
            ; etype = Option.map (M.string_of_item sys) item }
          in
          (* Add section to download Dedukti file *)
          let exp =
            { Jt.system = "dedukti"
            ; file = DkTools.get_file mdl
            ; etype = None }
            :: (List.map art2exp !Systems.artefact_path) in
          begin match e with
            | E.Decl(_,_,_,t) ->
              let ppt_term =  ppt_of_dkterm mdl acc t in
              { name = uri
              ; taxonomy = M.string_of_tx tx
              ; term = ppt_term
              ; term_opt = None
              ; label
              ; deps = List.map Uri.to_string deps
              ; theory = []
              ; exp } :: (loop acc tl)
            | E.Def(_,_,_,teo,te)  ->
              (* We use lazy to remap the computation, and avoid computing the
                 ppterm to finally discard it. *)
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
              ; deps = List.map Uri.to_string deps
              ; theory = []
              ; exp } :: (loop acc tl)
            | _                     -> loop acc tl
          end
        | _ -> loop acc tl
    in
    loop init entries

  let print_document : Jt.document pp = fun fmt doc ->
    Jt.document_to_yojson doc |> Yojson.Safe.pretty_print fmt
end
