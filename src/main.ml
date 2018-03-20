open Basic
open Term
open Entry
open Parser

let handle_entry md e =
  match e with
  | Decl(lc,id,st,ty) ->
      begin
        match Env.declare lc id st ty with
        | OK () -> Compile.compile_declaration (mk_name md id) ty
        | Err e -> Errors.fail_env_error e
      end
  | Def(lc,id,opaque,Some ty,te) ->
      begin
        let define = if opaque then Env.define_op else Env.define in
        match define lc id te (Some ty) with
        | OK () -> Compile.compile_definition (mk_name md id) ty te
        | Err e -> Errors.fail_env_error e
      end
  | Def _   -> failwith "Definition without types are not supported"
  | Rules _ -> failwith "Rules are not part of the sttforall logic"
  | _       -> failwith "Commands are not supported"

let run_on_file file =
  let md = Env.init file in
  Confluence.initialize ();
  let input = open_in file in
  let entries = Parser.parse_channel md input in
  close_in input;
  let ast = List.map (handle_entry md) entries in
  if not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_mident md;
  Confluence.finalize ();
  Errors.success "File '%s' was successfully checked." file;
  let file = (try Filename.chop_extension file with _ -> file) ^ ".stt" in
  let oc = open_out file in
  Printf.fprintf oc "%a%!" Print.print_ast ast;
  close_out oc

let _ =
  let options = Arg.align
    [ ( "-I"
      , Arg.String Basic.add_path
      , " Add folder to Dedukti path" ) ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try List.iter run_on_file files with
  | Parse_error(loc,msg) ->
      let (l,c) = of_loc loc in
      Printf.eprintf "Parse error at (%i,%i): %s\n" l c msg;
      exit 1
  | Failure err          -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Sys_error err        -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit                 -> exit 3
