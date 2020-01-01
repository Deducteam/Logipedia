open Core
open Extras
open Console
open Json

(** Input Dedukti files. *)
let infiles : string list ref = ref []

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
    let module S = Systems in
    let f (name, system) =
      ( Format.sprintf "--%s" name
      , Arg.String (fun s -> S.artefact_path := (system, s) :: !S.artefact_path)
      , Format.sprintf " Output folder of system %s" name)
    in
    List.map f S.sys_spec
  in
  Arg.align @@ Cli.options @ sys_exps @
  [ ( "-J"
    , Arg.Set_string Compile.json_include
    , " Add folder to Json built files path" )
  ; ( "-m"
    , Arg.Set_string middleware
    , m_doc )
  ; ( "--debug"
    , Arg.Set_int log_level
    , " Enable debug mode" ) ] |>
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
  let files =
    !infiles @
    if !Cli.indir <> "" then
      let open Filename in
      Sys.readdir !Cli.indir |> Array.to_seq |>
      Seq.filter (fun f -> String.equal (extension f) ".dk") |>
      Seq.map (fun f -> !Cli.indir </> f) |>
      List.of_seq
    else []
  in
  let outdir = Option.get !Cli.outdir in
  (* Create output dir if it does not exist. *)
  if not (Sys.file_exists outdir) then Unix.mkdir_rec outdir 0o755;
  let module Denv = Api.Env.Default in
  let rules =
    let (module M) = Middleware.of_string !middleware in
    let module JsExp = Compile.Make(M) in
    Makefile.rules_for (module JsExp: Json.Compile.S) files
  in
  let module B = Build.Classic in
  let open Json.Makefile in
  if !log_level > 0 then Format.printf "%a@\n" (B.pp_rules pp_key) rules;
  let build = B.build ~key_eq ~valid_stored in
  (* [build] is now memoized: rules are not run twice. *)
  let build target =
    match build rules target with
    | Ok(_)    -> ()
    | Error(k) -> Format.printf "No rule to make %a@." pp_key k
  in
  try want files |> List.iter build
  with e ->
    let module Derr = Api.Errors.Make(Denv) in
    raise (Derr.graceful_fail None e)
