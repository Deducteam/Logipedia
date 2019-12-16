open Core
open Extras
open Json
module B = Kernel.Basic
module P = Parsing.Parser
module S = Systems
module Denv = Api.Env.Default
module Derr = Api.Errors.Make(Denv)

(** File into which exported file are written. *)
let output_dir = ref None

(** Input Dedukti files. *)
let infiles : string list ref = ref []

(** Directory with files to convert. *)
let indir : string ref = ref ""

(** The middleware used. *)
let middleware : string ref = ref ""

(** Options of command line. *)
let options =
  let m_doc =
    (* Documentation string for middlewares. *)
    let available_mid =
      "dummy" :: List.map fst Middleware.spec |> String.concat ", "
    in
    Format.sprintf " Middleware to use, one of %s" available_mid
  in
  let sys_exps =
    (* Spec list for export systems. *)
    let f (name, system) =
      ( Format.sprintf "--%s" name
      , Arg.String (fun s -> S.artefact_path := (system, s) :: !S.artefact_path)
      , Format.sprintf " Output folder of system %s" name)
    in
    List.map f S.sys_spec
  in
  Arg.align @@
    sys_exps @
    [ ( "-I"
      , Arg.String B.add_path
      , " Add folder to Dedukti path" )
    ; ( "-J"
      , Arg.Set_string Compile.json_include
      , " Add folder to Json built files path" )
    ; ( "-m"
      , Arg.Set_string middleware
      , m_doc )
    ; ( "-d"
      , Arg.Set_string indir
      , " Add directory containing Dedukti files to convert" )
    ; ( "-o"
      , Arg.String (fun s -> output_dir := Some(s))
      , " Set output directory" ) ] |>
  List.sort (fun (t,_,_) (u,_,_) -> String.compare t u)

(** [anon f] adds file [f] to the list of input dedukti files {!val:infiles}. *)
let anon : string -> unit = fun f -> infiles := !infiles @ [f]

let _ =
  let usage = Format.sprintf "Usage: %s [OPTIONS] FILES@\n" Sys.argv.(0) in
  begin
    try Arg.parse options anon usage;
    with Arg.Bad(s) ->
      Format.printf "%s@\n" s;
      Arg.usage options usage
  end;
  let dirfiles =
    if !indir <> "" then
      Sys.readdir !indir |> Array.to_seq |>
      Seq.filter (fun f -> String.equal (Filename.extension f) ".dk") |>
      Seq.map (fun f -> Filename.concat !indir f) |>
      List.of_seq
    else []
  in
  let rules =
    let (module M) = Middleware.of_string !middleware in
    let module JsExp = Compile.Make(M) in
    let prod file =
      Produce.rulem_of_file (module JsExp) file (Option.get !output_dir)
    in
    Produce.rulem_dk_idle :: List.map prod (!infiles @ dirfiles)
  in
  Format.printf "%a@\n" (Build.pp_rules Produce.pp_key) rules;
  let build target =
    match Build.buildm Produce.key_eq rules target with
    | Ok(_)      -> ()
    | Error(key) ->
      Format.printf "No rule to make %a@." Produce.pp_key key
  in
  try
    List.map (fun f -> Produce.JsMd(Denv.init f)) (!infiles @ dirfiles) |>
    List.iter build
  with e -> raise (Derr.graceful_fail None e)
