(** [log_enabled] set to true to enable debugging. *)
let log_enabled : bool ref = ref false

(** A shorter name for a format. *)
type 'a outfmt = ('a, Format.formatter, unit) format -> 'a

(** Main output formatter. *)
let out_fmt : Format.formatter ref = ref Format.std_formatter

(** [log fmt] logs to {!val:out_fmt}. *)
let log : 'a outfmt = fun fmt ->
  Format.fprintf !out_fmt ("@[" ^^ fmt ^^ "@]@.")
