open Extras
module B = Basic
module P = Parser

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
  { Ast.md = B.string_of_mident md; Ast.dep; items }

(* Export the file for the chosen system. *)
let export_system file =
  let md = Denv.init file in
  let input = open_in file in
  let entries = P.Parse_channel.parse md input in
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
  let entries = P.Parse_channel.parse md input in
  close_in input;
  let document = Json.doc_of_entries md entries in
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
          , Arg.String B.add_path
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
      ( Json_types.json_dir :=
          begin match !(output_file) with
            | None -> failwith "Output must be set with json"
            | Some(o) -> Filename.dirname o
          end
      ; List.iter export_json files )
    else
      List.iter export_system files
  with e -> Derr.graceful_fail None e
