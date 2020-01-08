(** Export Dedukti encoded in STTfa to systems. *)
open Core
open Console
open Extras

(** Input dedukti files. *)
let infiles : string list ref = ref []

(** Options list, redefined according to first argument. *)
let options : (string * Arg.spec * string) list ref = ref []

(** Which system to export to. *)
let export_mode : Systems.system option ref = ref None

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
let get_additional_opts : Systems.system -> (string * Arg.spec * string) list =
  function
  | `Pvs -> pvs_opts
  | _    -> sys_opts

(** [anon arg] process anonymous argument [arg]. The first anonymous argument is
    supposed to be the export mode. *)
let anon arg =
  match !export_mode with
  | Some(_) -> infiles := !infiles @ [arg]
  | None    ->
    (* Export mode is not set: set it. *)
    try
      let sy = Systems.system_of_string arg in
      export_mode := Some(sy);
      let sys_opts = get_additional_opts sy in
      options := Arg.align (sys_opts @ Cli.options)
    with Systems.UnsupportedSystem(s) ->
      let msg = Format.sprintf "Can't export to %s: system not supported" s in
      raise (Arg.Bad msg)

(** [get_system sys] returns the system module from a system
    identifier [sys]. *)
let get_system : Systems.system -> (module Export.S) = fun sy ->
  match sy with
  | `Pvs        -> (module Pvs)
  | `Hollight   -> (module Hollight)
  | `Lean       -> (module Lean)
  | `Coq        -> (module Coq)
  | `Matita     -> (module Matita)
  | `OpenTheory -> (module Opentheory)

let _ =
  let available_sys = List.map fst Systems.sys_spec |> String.concat ", " in
  let usage = Format.sprintf
      "Usage: %s EXPORT [OPTIONS]...\n\
\twith EXPORT being one of: %s\n\
Use %s EXPORT --help for help on an export command\n\
Available options for the selected mode:"
      Sys.argv.(0) available_sys Sys.argv.(0)
  in
  let module Denv = Api.Env.Default in
  try
    Arg.parse_dynamic options anon usage;
    match !export_mode with
    | None       -> raise @@ Arg.Bad "Missing export"
    | Some(syst) ->
      (* Get all the input files. *)
      let files =
        !infiles @
        if !Cli.indir <> "" then
          Sys.readdir !Cli.indir |> Array.to_seq |>
          Seq.filter (fun f -> String.equal (Filename.extension f) ".dk") |>
          Seq.map (Filename.concat !Cli.indir) |> List.of_seq
        else []
      in
      let outdir = Option.get !Cli.outdir in
      (* Create output dir if it does not exist. *)
      if not (Sys.file_exists outdir) then Unix.mkdir_rec outdir 0o755;
      let (module Syst) = get_system syst in
      let open Syst.Makefile in
      let module B = Build.Classic in
      (* Create the needed rules. *)
      let rules = rules_for files in
      if !Cli.write_depends then Format.printf "%a@." (B.pp_rules pp_key) rules;
      let build = B.build ~key_eq ".sttfa2sys.db" ~valid_stored in
      let build target =
        match build rules target with
        | Ok(_)      -> ()
        | Error(key) -> Format.printf "No rule to make %a@." pp_key key
      in
      want files |> List.iter build
  with
  | Arg.Bad(s) ->
    Format.printf "%s\n" s;
    Arg.usage (Arg.align Cli.options) usage
  | e          ->
    let module Derr = Api.Errors.Make(Denv) in
    raise (Derr.graceful_fail None e)
