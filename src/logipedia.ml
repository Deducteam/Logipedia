module B = Kernel.Basic
module D = Deps
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

(** The middleware used. *)
let middleware : string ref = ref ""

(** Options list, redefined according to first argument. *)
let options : (string * Arg.spec * string) list ref = ref []

(** Which system to export to. *)
let export_mode : S.system option ref = ref None

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

(** Options for any system export. --fast : does ont compute trace *)
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
    try
      export_mode := Some(S.system_of_string arg);
      options := Arg.align (sys_opts @ common_opts)
    with S.UnsupportedSystem(s) ->
      let msg = Format.sprintf "Can't export to %s: system not supported" s in
      raise (Arg.Bad msg)

let export_file ast system =
  let (module M:Export.E) = Export.of_system system in
  let (fmt, file) =
    match !output_file with
    | None    -> Format.std_formatter, None
    | Some(f) ->
      let file = open_out f in
      (Format.formatter_of_out_channel file, Some(file))
  in
  M.print_ast fmt ast;
  match file with
  | None    -> ()
  | Some(f) -> close_out f


(* Compute an STTforall ast from Dedukti entries *)
let mk_ast md entries =
  let items = List.map (Compile.compile_entry md) entries in
  let fold_entry_dep dep e = D.QSet.union dep
      (Deps.dep_of_entry [Sttfadk.sttfa_module;md] e) in
  let dep = List.fold_left fold_entry_dep D.QSet.empty entries in
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

let _ =
  let available_sys = List.map fst S.sys_spec |> String.concat ", " in
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
    | Some(s) ->
      if !infile = "" then raise (Arg.Bad "Input file required");
      export_system s !infile
  with
  | Arg.Bad(s) ->
    Format.printf "%s\n" s;
    Arg.usage (Arg.align common_opts) usage
  | e          -> raise e
