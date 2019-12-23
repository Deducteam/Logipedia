(** [log_enabled] set to true to enable debugging. *)
let log_enabled : bool ref = ref false

(** A shorter name for a format. *)
type 'a outfmt = ('a, Format.formatter, unit) format

(** Main output formatter. *)
let out_fmt : Format.formatter ref = ref Format.std_formatter

(** Type of a logger. *)
type 'a logger = {logger : 'a. 'a outfmt -> 'a}

(** Defines a logging function. *)
type logger_data =
  { logger_name : string
  (** A name to identify who logged what. *) }

(** The registered loggers. *)
let loggers : logger_data list ref = ref []

(** [new_logger name] creates and registers a new logger. *)
let new_logger : string -> 'a logger = fun name ->
  if String.length name <> 4 then invalid_arg "new_logger";
  loggers := {logger_name=name} :: !loggers;
  { logger = fun fmt ->
        Format.fprintf !out_fmt ("[%s] " ^^ fmt ^^ "@.") name }
