open Core
open Extras
open Console
open Json

(** Input Dedukti files. *)
let infiles : string list ref = ref []

(** Options of command line. *)
let options =
  let sys_exps =
    (* Spec list for export systems. *)
    let module S = Systems in
    let f (name, system) =
      ( Format.sprintf "--%s" name
      , Arg.String (fun s -> S.artefact_path := (system, s) :: !S.artefact_path)
      , Format.sprintf " Output folder of system %s" name)
    in
    List.map f S.spec
  in
  Cli.options @ sys_exps @
  ( "-J"
  , Arg.Set_string Compile.json_include
  , " Add folder to Json built files path" ) :: Middleware.options
  |> List.sort (fun (t,_,_) (u,_,_) -> String.compare t u)
  |> Arg.align

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
    !infiles @ if !Cli.indir <> "" then Cli.dks_in !Cli.indir else []
  in
  let outdir = Option.get !Cli.outdir in
  (* Create output dir if it does not exist. *)
  if not (Sys.file_exists outdir) then Unix.mkdir_rec outdir 0o755;
  let module Denv = Api.Env.Default in
  let generators =
    let (module M) = Middleware.of_string !Cli.middleware in
    let module JsExp = Compile.Make(M) in
    Makefile.mk_generators (module JsExp)
  in
  let module B = Build.Classic in
  let open Json.Makefile in
  (* FIXME remove? *)
  if !Cli.write_depends then Format.printf "%a@\n" (B.pp_rules pp_key) [];
  let build = B.build ~key_eq ".json" ~valid_stored in
  let build target =
    match build ~generators [] target with
    | Ok(_)    -> ()
    | Error(k) -> exit_with "No rule to make %a@." pp_key k
  in
  try want files |> List.iter build
  with e ->
    let module Derr = Api.Errors.Make(Denv) in
    raise (Derr.graceful_fail None e)
