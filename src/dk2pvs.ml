(** PVS exporter module. *)
module B = Kernel.Basic
module E = Export

(** File into which exported file are written. *)
let output_file = ref None

(** Input dedukti files. *)
let infile : string ref = ref ""

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
      , " Set output file" ) ]

let _ =
  let usage = Format.sprintf "Usage: %s [OPTIONS]...@\n" Sys.argv.(0) in
  let anon _ = raise (Arg.Bad "No anonymous argument") in
  begin
    try Arg.parse options anon usage;
    with Arg.Bad(s) ->
      Format.printf "%s@\n" s;
      Arg.usage options usage
  end;
  let outfmt, ochan =
    match !output_file with
    | None    -> Format.std_formatter, None
    | Some(f) ->
      let ochan = open_out f in
      Format.formatter_of_out_channel ochan, Some(ochan)
  in
  E.export_system (module Pvs) !infile outfmt;
  match ochan with
  | None     -> ()
  | Some(oc) -> close_out oc
