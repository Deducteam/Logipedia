module B = Kernel.Basic
module D = Core.Deps
module P = Parsing.Parser
module S = Core.Systems

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
      , Arg.Set Sttfa.Sttfatyping.Tracer.fast
      , " Fast" ) ]

(** Whether to generate PVS top file. *)
let pvs_gen_top : bool ref = ref false

(** Options for PVS. *)
let pvs_opts =
  [ ( "--gen-top"
    , Arg.Set pvs_gen_top
    , " Generate PVS top file" ) ]

(** [get_additional_opts sy] returns additional cli options for a
    system [sy]. *)
let get_additional_opts : S.system -> (string * Arg.spec * string) list =
  function
  | `Pvs -> pvs_opts
  | _    -> sys_opts

(** [anon arg] process anonymous argument [arg]. The first anonymous argument is
    supposed to be the export mode. *)
let anon arg =
  match !export_mode with
  | Some(_) -> raise (Arg.Bad "Too many anonymous arguments provided")
  | None    ->
    (* Export mode is not set: set it. *)
    try
      let sy = S.system_of_string arg in
      export_mode := Some(sy);
      let sys_opts = get_additional_opts sy in
      options := Arg.align (sys_opts @ common_opts)
    with S.UnsupportedSystem(s) ->
      let msg = Format.sprintf "Can't export to %s: system not supported" s in
      raise (Arg.Bad msg)

(** [get_system sys] returns the system module from a system
    identifier [sys]. *)
let get_system : S.system -> (module Export.S) = fun sy ->
  match sy with
  | `Pvs      -> (module Pvs)
  | `Hollight -> (module Hollight)
  | `Lean     -> (module Lean)
  | `Coq      -> (module Coq)
  | `Matita   -> (module Matita)
  | _         -> failwith "Not yet implemented"

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
      let outfmt, ochan =
        match !output_file with
        | None    -> Format.std_formatter, None
        | Some(f) ->
          let ochan = open_out f in
          Format.formatter_of_out_channel ochan, Some(ochan)
      in
      if !infile = "" then raise (Arg.Bad "Input file required");
      let (module Sys) = get_system s in
      let md = Denv.init !infile in
      let input = open_in !infile in
      let entries = P.Parse_channel.parse md input in
      close_in input;
      let ast = Sys.Ast.compile md entries in
      Sys.export ast outfmt;
      match ochan with
      | None     -> ()
      | Some(oc) -> close_out oc
  with
  | Arg.Bad(s) ->
    Format.printf "%s\n" s;
    Arg.usage (Arg.align common_opts) usage
  | e          -> raise e
