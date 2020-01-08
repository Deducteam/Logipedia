open Extras
open Console
open Kernel.Basic
open Kernel.Term
open Parsing.Entry
open Kernel.Rule

module D = Api.Dep
module E = Api.Env.Make(Kernel.Reduction.Default)
module ErrorHandler = Api.Errors.Make(E)

let log_dep = new_logger "deps"
let log_dep = log_dep.logger

(** {1 Dedukti dependency computation} *)

module QSet = Set.Make(String)

let add_dep : mident -> QSet.t =
 fun md -> QSet.singleton (string_of_mident @@ md)

let qset_of_list f l =
  List.fold_left (fun set x -> QSet.union (f x) set) QSet.empty l

(** Term / pattern / entry traversal commands. *)

let rec mk_term t =
  match t with
  | Kind | Type _ | DB _ -> QSet.empty
  | Const (_, c) -> add_dep (md c)
  | App (f, a, args) -> qset_of_list mk_term (f :: a :: args)
  | Lam (_, _, None, te) -> mk_term te
  | Lam (_, _, Some ty, te) -> QSet.union (mk_term ty) (mk_term te)
  | Pi (_, _, a, b) -> QSet.union (mk_term a) (mk_term b)

let rec mk_pattern p =
  match p with
  | Var (_, _, _, args) -> qset_of_list mk_pattern args
  | Pattern (_, c, args) ->
      QSet.union (add_dep (md c)) (qset_of_list mk_pattern args)
  | Lambda (_, _, te) -> mk_pattern te
  | Brackets t -> mk_term t


let mk_rule r = QSet.union (mk_pattern r.pat) (mk_term r.rhs)

let dep_of_entry = function
  | Decl (_, _, _, te) -> mk_term te
  | Def (_, _, _, None, te) -> mk_term te
  | Def (_, _, _, Some ty, te) -> QSet.union (mk_term ty) (mk_term te)
  | Rules (_, rs) -> qset_of_list mk_rule rs
  | Eval (_, _, te) -> mk_term te
  | Infer (_, _, te) -> mk_term te
  | Check (_, _, _, Convert (t1, t2)) -> QSet.union (mk_term t1) (mk_term t2)
  | Check (_, _, _, HasType (te, ty)) -> QSet.union (mk_term te) (mk_term ty)
  | DTree (_, _, _) -> QSet.empty
  | Print (_, _) -> QSet.empty
  | Name (_, _) -> QSet.empty
  | Require (_, md) -> add_dep md

let dep_of_entry (mds:mident list) e =
  List.fold_left (fun qset md -> QSet.remove (string_of_mident md) qset)
    (dep_of_entry e) mds

let deps_of_entry : mident -> entry -> name list = fun mid e ->
  let id = match e with
    | Decl(_,id,_,_)
    | Def(_,id,_,_,_) -> id
    | _               -> assert false
  in
  D.compute_ideps := true; (* Compute dependencies of items *)
  D.make mid [e];
  let name = mk_name mid id in
  try D.(NameSet.elements (get_data name).down)
  with D.(Dep_error(NameNotFound(_))) -> []

let deps_of_md : mident -> mident list =
  fun md ->
  log_dep ~lvl:4 "of [%a]" Api.Pp.Default.print_mident md;
  let file = Api.Dep.get_file md in
  let inchan = open_in file in
  Api.Dep.compute_ideps := false;
  let entries = Parsing.Parser.Parse_channel.parse md inchan in
  close_in inchan;
  begin
    try Api.Dep.make md entries
    with e -> ErrorHandler.graceful_fail None e
  end;
  let deps = Hashtbl.find Api.Dep.deps md in
  Api.Dep.MDepSet.to_seq deps.deps |> Seq.map fst |> List.of_seq

(** Use build system to compute dependencies. *)
module Compute = struct
  open Build.Classic

  let log_rule = Build.log_rule.logger

  type key = DkTools.mident
  type value = DkTools.mident list * string * float

  let key_eq = DkTools.mident_eq

  let pp_key = DkTools.pp_mident

  let valid_stored : key -> value -> bool = fun md (_,pth,t) ->
    (* Assert that the path matches the module *)
    Filename.(!/pth) = Kernel.Basic.string_of_mident md &&
    let file_modif = mtime pth in
    file_modif < t

  let compute : key -> (key, value) rule = fun md ->
    let cp _ =
      log_rule ~lvl:4 "deps of [%a]" pp_key md;
      let t = Unix.time () in
      deps_of_md md, DkTools.get_file md, t
    in
    target md +> cp

  let build = build ~key_eq ".logideps.db" ~valid_stored

  let deps_of_md : mident -> mident list = fun md ->
    match build [compute md] md with
    | Ok(mds,_,_) -> mds
    | Error(md) ->
      exit_with "Couldn't compute dependencies of %a"
        Api.Pp.Default.print_mident md
end

let deps_of_md = Compute.deps_of_md
