(** Export Dedukti encoded in STTfa to systems. *)
open Core
open Console
open Extras

(** File into which exported file are written. *)
let outdir = ref None

(** Input dedukti files. *)
let infiles : string list ref = ref []

(** Input Dedukti directory. *)
let indir : string ref = ref ""

(** Options list, redefined according to first argument. *)
let options : (string * Arg.spec * string) list ref = ref []

(** Which system to export to. *)
let export_mode : Systems.system option ref = ref None

(** Options common to both modes. *)
let common_opts =
  [ ( "-I"
    , Arg.String Kernel.Basic.add_path
    , " Add folder to Dedukti path" )
  ; ( "-d"
    , Arg.Set_string indir
    , " Add directory containing Dedukti files to convert" )
  ; ( "--debug"
    , Arg.Set log_enabled
    , " Enable debug mode" )
  ; ( "-o"
    , Arg.String (fun s -> outdir := Some(s))
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
      options := Arg.align (sys_opts @ common_opts)
    with Systems.UnsupportedSystem(s) ->
      let msg = Format.sprintf "Can't export to %s: system not supported" s in
      raise (Arg.Bad msg)

(** [get_system sys] returns the system module from a system
    identifier [sys]. *)
let get_system : Systems.system -> (module Export.S) = fun sy ->
  match sy with
  | `Pvs -> (module Pvs)
  | `Hollight -> (module Hollight)
  | `Lean -> (module Lean)
  | _    -> failwith "Not yet implemented"

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
      let dirfiles =
        if !indir <> "" then
          Sys.readdir !indir |> Array.to_seq |>
          Seq.filter (fun f -> String.equal (Filename.extension f) ".dk") |>
          Seq.map (Filename.concat !indir) |> List.of_seq
        else []
      in
      let rules =
        (* Create the needed rules *)
        let (module Syst) = get_system syst in
        let prod file =
          let md = Denv.init file in
          let entries_pp : Parsing.Entry.entry list pp = fun fmt entries ->
            let ast = Syst.Ast.compile md entries in
            Syst.export fmt ast
          in
          let ext = List.assoc syst Core.Systems.sys_ext in
          let outdir = Option.get !outdir in
          [ Build_template.mk_rule_sys_of_dk ~entries_pp md ext outdir
          ; Build_template.mk_rule_sig md ]
        in
        Build_template.mk_rule_sig (Kernel.Basic.mk_mident "sttfa")
        :: (List.map prod (!infiles @ dirfiles) |> List.flatten)
      in
      if !log_enabled then
        log "%a@." (Build.pp_rulems Build_template.pp_key) rules;
      let build = Build.buildm Build_template.key_eq in
      let build target =
        match build rules target with
        | Ok(_)      -> ()
        | Error(key) ->
          Format.printf "No rule to make %a@." Build_template.pp_key key
      in
      let package file = `SysMd(Denv.init file) in
      List.map package (!infiles @ dirfiles) |> List.iter build
  with
  | Arg.Bad(s) ->
    Format.printf "%s\n" s;
    Arg.usage (Arg.align common_opts) usage
  | e          ->
    let module Derr = Api.Errors.Make(Denv) in
    raise (Derr.graceful_fail None e)
