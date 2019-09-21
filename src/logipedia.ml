open Basic
open Parser
open Extras

module Denv = Env.Default
module Derr = Errors.Make(Denv)

let err_msg fmt =
  Format.eprintf "%s" ("\027[31m" ^ "[ERROR] " ^ "\027[m");
  Format.eprintf fmt

let system : Systems.system ref = ref (`Coq)

let set_export s =
  system := Systems.system_of_string s

let output_file = ref None

let set_output_file s =
  output_file := Some s

let export_file ast system =
  let (module M:Export.E) = Export.of_system system in
  let fmt =
    match !output_file with
    | None -> Format.std_formatter
    | Some f ->
      Format.formatter_of_out_channel (open_out f)
  in
  (* FIXME: The file is not closed, is this a problem? *)
  M.print_ast fmt ast

(* Compute an STTforall ast from Dedukti entries *)
let mk_ast md entries =
  let items = List.map (Compile.compile_entry md) entries in
  let fold_entry_dep dep e = Ast.QSet.union dep
      (Deps.dep_of_entry [Sttfadk.sttfa_module;md] e) in
  let dep = List.fold_left fold_entry_dep Ast.QSet.empty entries in
  { Ast.md = string_of_mident md; Ast.dep; items }

(* Export the file for the chosen system. *)
let export_system file =
  let md = Denv.init file in
  let input = open_in file in
  let entries = Parse_channel.parse md input in
  close_in input;
  begin
    let sttfa_ast = mk_ast md entries in
    let (module M:Export.E) = Export.of_system !system in
    export_file sttfa_ast !system;
  end

(* Json export is done without using the Sttfa AST. *)
let export_json file =
  let md = Denv.init file in
  let input = open_in file in
  let entries = Parse_channel.parse md input in
  close_in input;
  let taxons = Taxonomy.taxonomise md entries in
  let document = List.filter_map (Json.item_of_entry taxons md) entries in
  let fmt = match !output_file with
    | None    -> Format.std_formatter
    | Some(f) -> Format.formatter_of_out_channel (open_out f)
  in
  Json.print_document fmt document

let _ =
  try
    let to_json = ref false in
    let options =
      Arg.align
        [ ( "-I"
          , Arg.String Basic.add_path
          , " Add folder to Dedukti path" )
        ; ( "-o"
          , Arg.String set_output_file
          , " Set output file" )
        ; ( "--fast"
          , Arg.Set Sttfatyping.Tracer.fast
          , " Set output file" )
        ; ( "--export"
          , Arg.String set_export
          , " Set exporting system" )
        ; ( "--export-json"
          , Arg.Set to_json
          , " Generate json files" ) ]
    in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
    let usage = usage ^ "Available options:" in
    let files =
      let files = ref [] in
      Arg.parse options (fun f -> files := f :: !files) usage ;
      List.rev !files
    in
    if !to_json then
      List.iter export_json files
    else
      List.iter export_system files
 with
  | Env.EnvError (id,l,e) -> Derr.fail_env_error (id,l,e)
  | Failure err ->
      err_msg "Failure: %s@." err;
      exit 2
  | Sys_error err ->
      err_msg "System error: %s@." err;
      exit 3
  | Exit -> exit 4
