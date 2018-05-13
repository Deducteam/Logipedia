open Basic
open Term
open Entry
open Parser
open Rule
open Ast

let system : Export.system ref = ref (Export.(`Pvs))

let set_export s =
  if s = "coq" then
    system := Export.(`Coq)
  else if s = "matita" then
    system := Export.(`Matita)
  else if s = "ot" then
    system := Export.(`OpenTheory)
  else if s = "pvs" then
    system := Export.(`Pvs)
  else if s = "latex" then
    system := Export.(`Latex)
  else if s = "csv" then
    system := Export.(`Csv)
  else if s = "lean" then
    system := Export.(`Lean)
  else
    failwith (Format.sprintf "%s is not among the supported systems@." s)

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


let export_file file ast system =
  let basename = try Filename.chop_extension file with _ -> file in
  let (module M:Export.E) = Export.of_system system in
  let stt_file = basename ^ "." ^ M.extension in
  let oc = open_out stt_file in
  M.print_ast oc basename ast ;
  close_out oc

let run_on_file file =
  let md = Env.init file in
  Confluence.initialize () ;
  let input = open_in file in
  let entries = Parser.parse_channel md input in
  close_in input ;
  let items = List.map (handle_entry md) entries in
  let dep = List.fold_left (fun dep e -> QSet.union dep (Dep.dep_of_entry md e)) QSet.empty entries in
  let ast = {md = string_of_mident md; dep; items} in
  if not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_mident md ;
  Confluence.finalize () ;
  export_file file ast `OpenTheory

let _ =
  let options =
    Arg.align
      [ ("-I", Arg.String Basic.add_path, " Add folder to Dedukti path") ]
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
