open Basic
open Term
open Entry
open Parser
open Rule
open Ast

let set_export s = ()

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
  let dep = List.fold_left (fun dep e -> QSet.union dep (Dep.dep_of_entry md e)) QSet.empty entries in
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
      ; ("-to", Arg.String set_export, "Set the exporting system. Currently [ascii|coq|matita|pvs|tex|pvs] are supported" )
      ; ( "--with-types"
        , Arg.Set Print_pvs.with_types
        , " Print types on binders and in the proof judgments (tex and ascii only)" ) ]
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
