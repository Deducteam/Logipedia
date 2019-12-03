(** PVS exporter module. *)
module B = Kernel.Basic
module E = Export

(** File into which exported file are written. *)
let output_file = ref None

(** Input dedukti files. *)
let infile : string ref = ref ""

(** Whether to generate PVS top file. *)
let gen_top : bool ref = ref false

(** Options list, redefined according to first argument. *)
let options : (string * Arg.spec * string) list =
  Arg.align
    [ ( "-I"
      , Arg.String B.add_path
      , " Add folder to Dedukti path" )
    ; ( "-f"
      , Arg.Set_string infile
      , " Input Dedukti file" )
    ; ( "-o"
      , Arg.String (fun s -> output_file := Some(s))
      , " Set output file" )
    ; ( "--gen-top"
      , Arg.Set gen_top
      , " Generate PVS top file" ) ]

let _ =
  let usage = Format.sprintf "Usage: %s [OPTIONS]...@\n" Sys.argv.(0) in
  let files = ref [] in
  let anon e =
    if not !gen_top then raise (Arg.Bad "No anonymous argument") else
    files := e :: !files
  in
  begin
    try Arg.parse options anon usage;
    with Arg.Bad(s) ->
      Format.printf "%s@\n" s;
      Arg.usage options usage
  end;
  files := List.rev !files;
  let outfmt, ochan =
    match !output_file with
    | None    -> Format.std_formatter, None
    | Some(f) ->
      let ochan = open_out f in
      Format.formatter_of_out_channel ochan, Some(ochan)
  in
  if !gen_top then Pvs.gen_top !files outfmt else
  E.export_system (module Pvs) !infile outfmt;
  match ochan with
  | None     -> ()
  | Some(oc) -> close_out oc
