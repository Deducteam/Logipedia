(** Tools for the command line (e.g. logging). *)

(** {1 Logging} *)

(** Logging level, between 0 and anything. The lower the quieter. *)
let log_level : int ref = ref 0

(** A shorter name for a format. *)
type 'a outfmt = ('a, Format.formatter, unit) format

(** Formatter with continuation. *)
type ('a, 'b) koutfmt = ('a, Format.formatter, unit, unit, unit, 'b) format6

(** Main output formatter. *)
let out_fmt : Format.formatter ref = ref Format.std_formatter

(** [colour k fmt] adds colour [k] to format [fmt]. *)
let colour k fmt = "\027[" ^^ k ^^ "m" ^^ fmt ^^ "\027[0m@?"

let red fmt = colour "31" fmt
let cya fmt = colour "36" fmt

(** Type of a logger, a logger [log] is used as [log ?lvl msg] where [?lvl]
    is an optional level (set to 1 by default), and [msg] the message as an
    [outfmt]. *)
type 'a logger = {logger : 'a. ?lvl:int -> 'a outfmt -> 'a}

(** Defines a logging function. *)
type logger_data =
  { logger_name : string
  (** A name to identify who logged what. *) }

(** The registered loggers. *)
let loggers : logger_data list ref = ref []

(** [new_logger name] creates and registers a new logger with 4 letters name
    [name]. Loggers' output is {!val:out_fmt}. *)
let new_logger : string -> 'a logger = fun name ->
  if String.length name <> 4 then invalid_arg "new_logger";
  let check data =
    if name = data.logger_name then invalid_arg "used logger name"; ()
  in
  List.iter check !loggers;
  loggers := {logger_name=name} :: !loggers;
  { logger = fun ?(lvl=1) fmt ->
        let pp = Format.(if !log_level >= lvl then fprintf else ifprintf) in
        pp !out_fmt ((cya "[%s] ") ^^ fmt ^^ "@.") name }

(** [exit_with msg] aborts execution with error message [msg] and code 1. *)
let exit_with : ('a, 'b) koutfmt -> 'a = fun fmt ->
  Format.kfprintf (fun _ -> exit 1) Format.err_formatter (red (fmt ^^ "@."))

(** {2 Command line interface} *)

(** Some command line interface arguments. *)
module Cli =
struct
  (** Directory containing Dedukti files. *)
  let indir : string ref = ref ""

  (** Directory where to put translated files. We use a reference to avoid using
      uninitialised values. *)
  let outdir : string option ref = ref None

  (** Whether to write the dependency schema of the build system into a '.d'
      file. *)
  let write_depends : bool ref = ref false

  (** Options passed to dedukti. *)
  let dkopts : string ref = ref ""

  (** A command line argument specification. *)
  type t = string * Arg.spec * string

  (** [dks_in dir] returns the path of dk files in directory [dir]. *)
  let dks_in : string -> string list = fun dir ->
    Sys.readdir dir |> Array.to_seq |>
    Seq.filter (fun f -> String.equal (Filename.extension f) ".dk") |>
    Seq.map (Filename.concat dir) |> List.of_seq

  (** Common options. *)
  let options : t list =
    [ ( "-I"
      , Arg.String Kernel.Basic.add_path
      , " Add folder to Dedukti path" )
    ; ( "-d"
      , Arg.Set_string indir
      , " Add directory containing Dedukti files to convert" )
    ; ( "--debug"
      , Arg.Set_int log_level
      , " Enable debug mode" )
    ; ( "--depends"
      , Arg.Set write_depends
      , " Write dependencies of computations into a .d file" )
    ; ( "--dkopts"
      , Arg.Set_string dkopts
      , " Options passed to dedukti (to produce dko files)" )
    ; ( "-o"
      , Arg.String (fun s -> outdir := Some(s))
      , " Set output directory" ) ]
end
