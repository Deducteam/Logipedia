(** Export Dedukti encoded in STTfa to systems. *)
open Core
open Console
open Extras

(** Input dedukti files. *)
let infiles : string list ref = ref []

(** Options list, redefined according to first argument. *)
let options : (string * Arg.spec * string) list ref = ref []

(** Which system to export to. *)
let export_mode : Systems.t option ref = ref None

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
let get_additional_opts : Systems.t -> (string * Arg.spec * string) list =
  function
  | Pvs -> pvs_opts
  | _   -> sys_opts

(** [anon arg] process anonymous argument [arg]. The first anonymous argument is
    supposed to be the export mode. *)
let anon arg =
  match !export_mode with
  | Some(_) -> infiles := !infiles @ [arg]
  | None    ->
    (* Export mode is not set: set it. *)
    try
      let sy = Systems.of_string arg in
      export_mode := Some(sy);
      let sys_opts = get_additional_opts sy in
      options := Arg.align (sys_opts @ Middleware.options @ Cli.options)
    with Systems.UnsupportedSystem(s) ->
      let msg = Format.sprintf "Can't export to %s: system not supported" s in
      raise (Arg.Bad msg)

let _ =
  let available_sys = List.map fst Systems.spec |> String.concat ", " in
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
    | None            -> raise (Arg.Bad "Missing export")
    | Some(targetsys) ->
      (* Get all the input files. *)
      let files =
        !infiles @
        if !Cli.indir <> "" then Cli.dks_in !Cli.indir else []
      in
      let outdir = Option.get !Cli.outdir in
      (* Create output dir if it does not exist. *)
      if not (Sys.file_exists outdir) then Unix.mkdir_rec outdir 0o755;
      let (module Mid) = Middleware.of_string !Cli.middleware in
      let (module E) = Mid.get_exporter targetsys in
      (* Setting up build system. *)
      let module M = Export.GenBuildSys(E) in
      let build = M.(Build.Classic.build ~key_eq ".sttfaexp" ~valid_stored) in
      let build target =
        match build ~generators:M.generators M.rules target with
        | Ok(_)      -> ()
        | Error(key) -> Format.printf "No rule to make %a@." M.pp_key key
      in
      M.want files |> List.iter build
  with
  | Arg.Bad(s) ->
    Format.printf "%s\n" s;
    Arg.usage (Arg.align Cli.options) usage
  | e          ->
    let module Denv = Api.Env.Default in
    let module Derr = Api.Errors.Make(Denv) in
    raise (Derr.graceful_fail None e)
