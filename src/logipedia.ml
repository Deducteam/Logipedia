module B = Kernel.Basic
module P = Parsing.Parser
module S = Systems

module Denv = Api.Env.Default
module Derr = Api.Errors.Make(Denv)

(** System to which we export proofs. *)
let system : S.system ref = ref (`Coq)

(** File into which exported file are written. *)
let output_file = ref None

(** Input dedukti files. *)
let infile : string ref = ref ""

(** Options list, redefined according to first argument. *)
let options : (string * Arg.spec * string) list ref = ref []

(** Operation mode of logipedia. *)
type mode =
  | ModeJson
  (** Export to JSON *)
  | ModeSystem of S.system
  (** Export to a system *)

let export_mode : mode option ref = ref None

(** Options common to both modes. *)
let common_opts =
    [ ( "-I"
      , Arg.String B.add_path
      , " Add folder to Dedukti path" )
    ; ( "-f"
      , Arg.Set_string infile
      , " Input Dedukti file" )
    ; ( "-o"
      , Arg.String (fun s -> output_file := Some(s))
      , " Set output file" ) ]

(** Options for the json export. *)
let json_opts =
  let f (name, system) =
    ( Format.sprintf "--%s" name
    , Arg.String (fun s -> S.artefact_path := (system, s) :: !S.artefact_path)
    , Format.sprintf " Output folder of system %s" name )
  in
  List.map f S.sys_spec

(** Options for any system export. *)
let sys_opts =
    [ ( "--fast"
      , Arg.Set Sttfatyping.Tracer.fast
      , " Fast" ) ]

(** [anon arg] process anonymous argument [arg]. The first anonymous argument is
    supposed to be the export mode. *)
let anon arg =
  match !export_mode with
  | Some(_) -> raise (Arg.Bad "Too many anonymous arguments provided")
  | None    ->
    (* Export mode is not set: set it. *)
    if arg = "json"
    then
      ( export_mode := Some(ModeJson)
      ; options := Arg.align (json_opts @ common_opts) )
    else
      try
        ( export_mode := Some(ModeSystem(S.system_of_string arg))
        ; options := Arg.align (sys_opts @ common_opts) )
      with S.UnsupportedSystem(s) ->
        let msg = Format.sprintf "Can't export to %s: system not supported" s in
        raise (Arg.Bad msg)

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
let export_system syst file =
  let md = Denv.init file in
  let input = open_in file in
  let entries = P.Parse_channel.parse md input in
  close_in input;
  begin
    let sttfa_ast = mk_ast md entries in
    let (module M:Export.E) = Export.of_system syst in
    export_file sttfa_ast syst;
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
  let available_sys = "json" :: List.map fst S.sys_spec |> String.concat ", " in
  let usage = Format.sprintf
      "Usage: %s EXPORT [OPTIONS]...\n\
\twith EXPORT being one of: %s\n\
Use %s EXPORT --help for help on an export command\n\
Available options for the selected mode:"
      Sys.argv.(0) available_sys Sys.argv.(0)
  in
  try
    Arg.parse_dynamic options anon usage;
    match !export_mode with
    | None    -> raise @@ Arg.Bad "Missing export"
    | Some(m) ->
      if !infile = "" then raise (Arg.Bad "Input file required");
      begin
        match m with
        | ModeJson ->
          (* Some further argument processing for json. *)
          ( Json_types.json_dir :=
              begin match !(output_file) with
                | None    -> raise (Arg.Bad "Output must be set for json")
                | Some(o) -> Filename.dirname o
              end
          ; Json.basename := Filename.remove_extension !infile |>
                             Filename.basename
          ; export_json !infile )
        | ModeSystem(s) -> export_system s !infile
      end
  with
  | Arg.Bad(s) ->
    Format.printf "%s\n" s;
    Arg.usage (Arg.align common_opts) usage
  | e          -> raise e
