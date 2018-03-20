open Basic
open Term
open Entry
open Parser

let mk_entry md e =
  match e with
  | Decl(lc,id,st,ty) ->
    begin
      match Env.declare lc id st ty with
      | OK () -> ignore(Compile.compile_declaration (mk_name md id) ty)
      | Err e -> Errors.fail_env_error e
    end
  | Def(lc,id,opaque,Some ty,te) ->
    begin
      let define = if opaque then Env.define_op else Env.define in
      match define lc id te (Some ty) with
      | OK () -> ignore(Compile.compile_definition (mk_name md id) ty te)
      | Err e -> Errors.fail_env_error e
    end
  | Def _ -> failwith "Definition without types are not supported"
  | Rules(rs) -> failwith "Rules are not part of the sttforall logic"
  | Name _ -> ()
  | _ -> failwith "Commands are not supported"

let run_on_file export file =
  let input = open_in file in
  debug 1 "Processing file '%s'..." file;
  let md = mk_mident file in
  Env.init md;
  Confluence.initialize ();
  handle_channel md (mk_entry md) input;
  Errors.success "File '%s' was successfully checked." file;
  if export && not (Env.export ()) then
    Errors.fail dloc "Fail to export module '%a'." pp_mident (Env.get_name ());
  Confluence.finalize ();
  close_in input


let _ =
  let export       = ref false in
  let options = [
    ( "-e"
      , Arg.Set export
      , " Generates an object file (\".dko\")" )] in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try
    List.iter (run_on_file !export) files;
  with
  | Parse_error(loc,msg) ->
      let (l,c) = of_loc loc in
      Printf.eprintf "Parse error at (%i,%i): %s\n" l c msg;
      exit 1
  | Sys_error err        -> Printf.eprintf "ERROR %s.\n" err; exit 1
  | Exit                 -> exit 3
