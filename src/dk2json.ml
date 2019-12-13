module B = Kernel.Basic
module P = Parsing.Parser
module S = Core.Systems
module Denv = Api.Env.Default

(** File into which exported file are written. *)
let output_dir = ref None

(** Input dedukti files. *)
let infile : string ref = ref ""

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
  Arg.align
    sys_exps @
    [ ( "-I"
      , Arg.String B.add_path
      , " Add folder to Dedukti path" )
    ; ( "-f"
      , Arg.Set_string infile
      , " Input Dedukti file" )
    ; ( "-m"
      , Arg.Set_string middleware
      , m_doc )
    ; ( "-o"
      , Arg.String (fun s -> output_dir := Some(s))
      , " Set output directory" ) ] |>
  List.sort (fun (t,_,_) (u,_,_) -> String.compare t u)

(* Json export is done without using the Sttfa AST. *)
let export_json file (module M : Middleware.S) =
  let md = Denv.init file in
  let input = open_in file in
  let entries = P.Parse_channel.parse md input in
  close_in input;
  let module JsExp = Json.Compile.Make(M) in
  let document = JsExp.doc_of_entries md entries in
  let fmt = match !output_dir with
    | None    -> Format.std_formatter
    | Some(f) -> Format.formatter_of_out_channel (open_out f)
  in
  JsExp.print_document fmt document

let export_json_new file (module M: Middleware.S) =
  let module JsExp = Json.Compile.Make(M) in
  let fmt, ochan = match !output_dir with
    | None    -> Format.std_formatter, None
    | Some(d) ->
      let ofile =
        d ^ (Filename.chop_extension (Filename.basename file)) ^
        ".json"
      in
      let ochan = open_out ofile in
      Format.formatter_of_out_channel ochan, Some(ochan)
  in
  let rule = Make_json.make_doc_rulem (module JsExp) fmt file in
  let noext = Filename.chop_extension file in
  Make_json.buildm [rule] (Make_json.JsMd(Kernel.Basic.mk_mident noext));
  match ochan with Some(oc) -> close_out oc | None -> ()

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
  Json.Json_types.json_dir :=
    begin match !output_dir with
      | None    -> raise (Arg.Bad "Output file required")
      | Some(o) -> Filename.dirname o
    end;
  Json.Compile.basename := Filename.remove_extension !infile
                           |> Filename.basename;
  match !infiles with
  | [] ->
    (* FIXME *)
    Kernel.Basic.add_path (Filename.dirname !infile);
    export_json_new !infile (Middleware.of_string !middleware)
  | _  ->
    List.iter
      (fun file ->
         Kernel.Basic.add_path (Filename.dirname file); (* Hmmm... *)
         export_json_new file (Middleware.of_string !middleware))
      !infiles
