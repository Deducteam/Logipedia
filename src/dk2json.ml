open Core
open Console
open Extras
open Json

(** File into which exported file are written. *)
let outdir = ref None

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
    let module S = Systems in
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
      , Arg.String Kernel.Basic.add_path
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
    ; ( "--debug"
      , Arg.Set log_enabled
      , " Enable debug mode" )
    ; ( "-o"
      , Arg.String (fun s -> outdir := Some(s))
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
  let files =
    !infiles @
    if !indir <> "" then
      Sys.readdir !indir |> Array.to_seq |>
      Seq.filter (fun f -> String.equal (Filename.extension f) ".dk") |>
      Seq.map (fun f -> Filename.concat !indir f) |>
      List.of_seq
    else []
  in
  let mk_target file =
    let open Filename in
    file |> basename |> chop_extension |>
    (fun x -> String.concat "." [x; "json"]) |> concat (Option.get !outdir)
  in
  let module Denv = Api.Env.Default in
  let open Build_template in
  let mds = List.map Denv.init files in
  let rules =
    let (module M) = Middleware.of_string !middleware in
    let module JsExp = Compile.Make(M) in
    let mk_rule file =
      let md = Denv.init file in
      let entries_pp fmt entries =
        JsExp.print_document fmt (JsExp.doc_of_entries md entries)
      in
      mk_sysrule ~target:(mk_target file) ~entries_pp md
    in
    List.map mk_sigrule mds @ List.map mk_rule files
  in
  if !log_enabled then Format.printf "%a@\n" (Build.pp_rulems pp_key) rules;
  let build = Build.buildm ~key_eq ~valid_stored in
  (* [build] is now memoized: rules are not run twice. *)
  let build target =
    match build rules target with
    | Ok(_)    -> ()
    | Error(k) -> Format.printf "No rule to make %a@." pp_key k
  in
  try
    List.map (fun f -> mk_target f |> want) files |>
    List.iter build
  with e ->
    let module Derr = Api.Errors.Make(Denv) in
    raise (Derr.graceful_fail None e)
