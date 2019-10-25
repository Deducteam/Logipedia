module B = Kernel.Basic
module P = Parsing.Parser

module Denv = Api.Env.Default
module Derr = Api.Errors.Make(Denv)

let jscoqp : string option ref = ref None

let err_msg s =
  Format.eprintf "%s" ("\027[31m" ^ "[ERROR] " ^ "\027[m");
  Format.eprintf "%s" s

(** System to which we export proofs. *)
let system : Systems.system ref = ref (`Coq)

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
  | ModeSystem of Systems.system
  (** Export to a system *)

let export_mode : mode option ref = ref None

(** Options common to both modes. *)
let common_opts = Arg.align
    [ ( "-I"
      , Arg.String B.add_path
      , " Add folder to Dedukti path" )
    ; ( "-f"
      , Arg.Set_string infile
      , " Input Dedukti file")
    ; ( "-o"
      , Arg.String (fun s -> output_file := Some(s))
      , " Set output file" ) ]

(** Options for the json export. *)
let json_opts = Arg.align
    [ ( "--coq"
      , Arg.String (fun s -> jscoqp := Some(s))
      , " Folder with coq files to be referenced" ) ]

(** Options for any system export. *)
let sys_opts = Arg.align
    [ ( "--fast"
      , Arg.Set Sttfatyping.Tracer.fast
      , " Fast" ) ]

(** [anon arg] process anonymous argument [arg]. The first anonymous argument is
    supposed to be the export mode. *)
let anon arg =
  match !export_mode with
  | Some(_) -> raise (Arg.Bad "Only one anonymous argument allowed")
  | None    ->
    (* Export mode is not set: set it. *)
    if arg = "json"
    then
      ( export_mode := Some(ModeJson)
      ; options := json_opts @ common_opts )
    else
      try
        ( export_mode := Some(ModeSystem(Systems.system_of_string arg))
        ; options := sys_opts @ common_opts )
      with Invalid_argument s -> raise (Arg.Bad s)


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
  let usage = Format.sprintf
      "Usage: %s EXPORT [OPTIONS]...\n \
      \twith export being one of: coq, matita, opentheory, pvs or lean\n \
      Available options:" Sys.argv.(0)
  in
  try
    Arg.parse_dynamic options anon usage;
    match !export_mode with
    | None -> raise @@ Arg.Bad "Export mode not given"
    | Some(ModeJson) ->
      ( Json_types.json_dir :=
          begin match !(output_file) with
            | None -> raise (Arg.Bad "Output must be set for json")
            | Some(o) -> Filename.dirname o
          end
      ; export_json !infile )
    | Some(ModeSystem(s)) -> export_system s !infile
  with
  | Arg.Bad s -> err_msg s
  | e         -> raise e
