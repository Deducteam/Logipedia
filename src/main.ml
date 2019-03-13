open Basic
open Parser
open Ast

let err_msg fmt =
  Format.eprintf "%s" ("\027[31m" ^ "ERROR" ^ "\027[m");
  Format.eprintf fmt

let system : Systems.system ref = ref (`Coq)

let set_export s =
  system := Systems.system_of_string s

(* Not used currently because only one theory is supported currently *)
let output_theory : Theories.theory ref = ref `Sttfa

let input_theory : Theories.theory ref = ref `Sttfa

let set_output_theory s =
  output_theory := Theories.theory_of_string s

let set_input_theory s =
  input_theory := Theories.theory_of_string s

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
  let fold_entry_dep dep e = QSet.union dep (Dep.dep_of_entry [Sttfadk.sttfa_module;md] e) in
  let dep = List.fold_left fold_entry_dep QSet.empty entries in
  {md = string_of_mident md; dep; items}

(* Export the file for the system choosen. *)
let export_system file =
  let md = Env.init file in
  let input = open_in file in
  let entries = Parse_channel.parse md input in
  close_in input;
  begin
    let sttfa_ast = mk_ast md entries in
    let (module M:Export.E) = Export.of_system !system in
    export_file sttfa_ast !system;
  end

(* Right now export stuff in the database *)
let export_web file =
  Environ.set_package file;
  let md = Env.init file in
  let input = open_in file in
  let entries = Parse_channel.parse md input in
  close_in input;
  Web.export_entries (mk_ast md entries)

let _ =
  try
    let to_web = ref false in
    let options =
      Arg.align
        [ ("-I", Arg.String Basic.add_path, " Add folder to Dedukti path") ;
          ("-o", Arg.String set_output_file, " Set output file") ;
          ("--export", Arg.String set_export, " Set exporting system") ;
          ("--export-web", Arg.Set to_web, " Generate informations for the website") ;
          ("--theory-output", Arg.String set_output_theory, " Set theory (default: STTFA)") ;
          ("--theory-input", Arg.String set_input_theory, " Set theory (default: STTFA)") ]
    in
    let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
    let usage = usage ^ "Available options:" in
    let files =
      let files = ref [] in
      Arg.parse options (fun f -> files := f :: !files) usage ;
      List.rev !files
    in
    if !to_web then
      List.iter export_web files
    else
      List.iter export_system files
 with
  | Env.EnvError (l,e) -> Errors.fail_env_error l e
  | Failure err ->
      err_msg "Failure: %s@." err;
      exit 2
  | Sys_error err ->
      err_msg "System error: %s@." err;
      exit 3
  | Exit -> exit 4
