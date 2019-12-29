(** Tools for the command line (e.g. logging). *)

(** Logging level, between 0 and anything. The lower the quieter. *)
let log_level : int ref = ref 0

(** A shorter name for a format. *)
type 'a outfmt = ('a, Format.formatter, unit) format

(** Formatter with continuation. *)
type ('a, 'b) koutfmt = ('a, Format.formatter, unit, unit, unit, 'b) format6

(** Main output formatter. *)
let out_fmt : Format.formatter ref = ref Format.std_formatter

(** [colour k fmt] adds colour [k] to format [fmt]. *)
let colour k fmt = "\027[" ^^ k ^^ "m" ^^ fmt ^^ "\027[0m@!"

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
  loggers := {logger_name=name} :: !loggers;
  { logger = fun ?(lvl=1) fmt ->
        let pp = Format.(if !log_level >= lvl then fprintf else ifprintf) in
        pp !out_fmt ((cya "[%s] ") ^^ fmt ^^ "@.") name }

let exit_with : ('a, 'b) koutfmt -> 'a = fun fmt ->
  Format.kfprintf (fun _ -> exit 1) Format.err_formatter (red (fmt ^^ "@."))
