module B = Kernel.Basic
module P = Parsers.Parser

module Env = Api.Env
module Err = Api.Errors

let err_msg fmt =
  Format.eprintf "%s" ("\027[31m" ^ "[ERROR] " ^ "\027[m");
  Format.eprintf fmt

let system : Systems.system ref = ref (`Coq)

let set_export s =
  system := Systems.system_of_string s

let output_file = ref None

let set_output_file s =
  output_file := Some s

let export_file dkenv ast system =
  let (module M:Export.Eo) = Export.mk_exporter dkenv system in
  let fmt =
    match !output_file with
    | None -> Format.std_formatter
    | Some f ->
      Format.formatter_of_out_channel (open_out f)
  in
  (* FIXME: The file is not closed, is this a problem? *)
  M.print_ast fmt ast

(* Compute an STTforall ast from Dedukti entries *)
let mk_ast input =
  let items = Api.Processor.handle_input input (module Compile.ItemsBuilder) in
  let dep = Api.Processor.handle_input input (module Compile.DepBuilder) in
  let md = P.md_of_input input in
  { Ast.md = B.string_of_mident md; Ast.dep = dep; items }

(* Export the file for the chosen system. *)
let export_system file =
  let input = P.input_from_file file in
  let dkenv = Env.init input in
  let sttfa_ast = mk_ast input in
  export_file dkenv sttfa_ast !system

(* Json export is done without using the Sttfa AST. *)
let export_json file =
  let input = P.input_from_file file in
  let document = Api.Processor.handle_input input (module Json.DocumentBuilder) in
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
          , Arg.String Api.Files.add_path
          , " Add folder to Dedukti path" )
        ; ( "-o"
          , Arg.String set_output_file
          , " Set output file" )
        ; ( "--fast"
          , Arg.Set Sttfatyping.fast
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
  with e -> raise e
