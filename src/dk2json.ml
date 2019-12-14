open Core.Extras
module B = Kernel.Basic
module P = Parsing.Parser
module S = Core.Systems
module Denv = Api.Env.Default
module Derr = Api.Errors.Make(Denv)

(** File into which exported file are written. *)
let output_dir = ref None

(** Input Dedukti files. *)
let infiles : string list ref = ref []

(** The middleware used. *)
let middleware : string ref = ref ""

(** Options of command line. *)
let options =
  let m_doc =
    let available_mid =
      "dummy" :: List.map fst Middleware.spec |> String.concat ", "
    in
    Format.sprintf " Middleware to use, one of %s" available_mid
  in
  let sys_exps =
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
      , Arg.Set_string Json.Compile.json_include
      , " Add folder to Json built files path" )
    ; ( "-m"
      , Arg.Set_string middleware
      , m_doc )
    ; ( "-o"
      , Arg.String (fun s -> output_dir := Some(s))
      , " Set output directory" ) ] |>
  List.sort (fun (t,_,_) (u,_,_) -> String.compare t u)

let export_json : string -> (module Middleware.S) ->
  (Make_json.key, unit) Make_json.rulem =
  fun file (module M: Middleware.S) ->
  let module JsExp = Json.Compile.Make(M) in
  Make_json.make_doc_rulem (module JsExp) file (Option.get !output_dir)
  (* let noext = Filename.chop_extension file in
   * Make_json.buildm [rule] (Make_json.JsMd(Kernel.Basic.mk_mident noext)); *)

(** [anon f] adds file [f] to the list of input dedukti files {!val:infiles}. *)
let anon : string -> unit = fun f -> infiles := !infiles @ [f]

let _ =
  let usage = Format.sprintf "Usage: %s [OPTIONS]...@\n" Sys.argv.(0) in
  begin
    try Arg.parse options anon usage;
    with Arg.Bad(s) ->
      Format.printf "%s@\n" s;
      Arg.usage options usage
  end;
  let rules =
    List.map
      (fun file -> export_json file (Middleware.of_string !middleware))
      !infiles
  in
  begin
    let f t =
      try Make_json.buildm rules t
      with Make_json.NoRuleToMakeTarget(t) ->
        let t = match t with JsMd(t) | DkMd(t) -> t in
        Format.printf "No rule to make %a\n" (Kernel.Basic.pp_mident) t
    in
    let targets = List.map (fun f -> Make_json.JsMd(Denv.init f)) !infiles in
    try
      List.iter f targets
    with e -> Derr.graceful_fail None e
  end
