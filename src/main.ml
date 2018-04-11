open Basic
open Term
open Entry
open Parser
open Rule
open Ast

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

let handle_entry e =
  match e with
  | Decl (_, _, _, te) -> mk_term te
  | Def (_, _, _, None, te) -> mk_term te
  | Def (_, _, _, Some ty, te) -> QSet.union (mk_term ty) (mk_term te)
  | Rules rs -> qset_of_list mk_rule rs
  | Eval (_, _, te) -> mk_term te
  | Infer (_, _, te) -> mk_term te
  | Check (_, _, _, Convert (t1, t2)) -> QSet.union (mk_term t1) (mk_term t2)
  | Check (_, _, _, HasType (te, ty)) -> QSet.union (mk_term te) (mk_term ty)
  | DTree (_, _, _) -> QSet.empty
  | Print (_, _) -> QSet.empty
  | Name (_, _) -> QSet.empty
  | Require (_, md) -> add_dep md


let compute_dep md l =
  QSet.remove (string_of_mident md) (qset_of_list handle_entry l)


let handle_entry md e =
  match e with
  | Decl (lc, id, st, ty) -> (
    match Env.declare lc id st ty with
    | OK () -> Compile.compile_declaration (mk_name md id) ty
    | Err e -> Errors.fail_env_error e )
  | Def (lc, id, opaque, Some ty, te) -> (
      let define = if opaque then Env.define_op else Env.define in
      match define lc id te (Some ty) with
      | OK () -> Compile.compile_definition (mk_name md id) ty te
      | Err e -> Errors.fail_env_error e )
  | Def _ -> failwith "Definition without types are not supported"
  | Rules _ -> failwith "Rules are not part of the sttforall logic"
  | _ -> failwith "Commands are not supported"


let run_on_file file =
  let md = Env.init file in
  Confluence.initialize () ;
  let input = open_in file in
  let entries = Parser.parse_channel md input in
  close_in input ;
  let items = List.map (handle_entry md) entries in
  let dep = compute_dep md entries in
  let ast = {dep; items} in
  if not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_mident md ;
  Confluence.finalize () ;
  Print_pvs.current_module := string_of_mident md ;
  let prefix = try Filename.chop_extension file with _ -> file in
  let stt_file = prefix ^ ".pvs" in
  let oc = open_out stt_file in
  Print_pvs.print_ast_pvs oc prefix ast ;
  close_out oc 


let _ =
  let options =
    Arg.align
      [ ("-I", Arg.String Basic.add_path, " Add folder to Dedukti path")
      ; ( "--with-types"
        , Arg.Set Print_pvs.with_types
        , "Print types on binders and in the proof judgments" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage ;
    List.rev !files
  in
  try List.iter run_on_file files with
  | Parse_error (loc, msg) ->
      let l, c = of_loc loc in
      Printf.eprintf "Parse error at (%i,%i): %s\n" l c msg ;
      exit 1
  | Failure err ->
      Printf.eprintf "ERROR %s.\n" err ;
      exit 1
  | Sys_error err ->
      Printf.eprintf "ERROR %s.\n" err ;
      exit 1
  | Exit -> exit 3
