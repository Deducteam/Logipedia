module B = Kernel.Basic
module P = Parsing.Parser
module S = Core.Systems
module Denv = Api.Env.Default

(** File into which exported file are written. *)
let output_file = ref None

(** Input dedukti files. *)
let infile : string ref = ref ""

(** Directory containing input dedukti files. *)
let indir : string ref = ref ""

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
  Arg.align
    sys_exps @
    [ ( "-I"
      , Arg.String B.add_path
      , " Add folder to Dedukti path" )
    ; ( "-d"
      , Arg.Set_string indir
      , " Input Dedukti directory" )
    ; ( "-f"
      , Arg.Set_string infile
      , " Input Dedukti file" )
    ; ( "-m"
      , Arg.Set_string middleware
      , m_doc )
    ; ( "-o"
      , Arg.String (fun s -> output_file := Some(s))
      , " Set output file" ) ] |>
  List.sort (fun (t,_,_) (u,_,_) -> String.compare t u)

(* Json export is done without using the Sttfa AST. *)
let export_json file (module M : Middleware.S) =
  let md = Denv.init file in
  let input = open_in file in
  let entries = P.Parse_channel.parse md input in
  close_in input;
  let module JsExp = Json.Make(M) in
  let document = JsExp.doc_of_entries !infile md entries in
  let fmt = match !output_file with
    | None    -> Format.std_formatter
    | Some(f) -> Format.formatter_of_out_channel (open_out f)
  in
  JsExp.print_document fmt document

let _ =
  let usage = Format.sprintf "Usage: %s [OPTIONS]...@\n" Sys.argv.(0) in
  let anon _ = raise (Arg.Bad "No anonymous argument") in
  begin
    try Arg.parse options anon usage;
    with Arg.Bad(s) ->
      Format.printf "%s@\n" s;
      Arg.usage options usage
  end;
  Json__Json_types.json_dir :=
    begin match !output_file with
      | None    -> raise (Arg.Bad "Output file required")
      | Some(o) -> Filename.dirname o
    end;
  Json.basename := Filename.remove_extension !infile |> Filename.basename;
  export_json !infile (Middleware.of_string !middleware)
