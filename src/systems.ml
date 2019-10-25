type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean ]

let systems = [`Coq ; `Matita ; `Pvs ; `OpenTheory ; `Lean ]

exception UnsupportedSystem of string

(** A specification is
    - an identifier,
    - a system *)
type spec = string * system

let sys_spec : spec list =
  [ ( "coq"       , `Coq        )
  ; ( "matita"    , `Matita     )
  ; ( "ot"        , `OpenTheory )
  ; ( "opentheory", `OpenTheory )
  ; ( "pvs"       , `Pvs        )
  ; ( "lean"      , `Lean       ) ] |>
  List.sort (fun (s,_) (t,_) -> String.compare s t)

(** [system_of_string str] returns the system associated to the string [str]. *)
let system_of_string : string -> system = fun s ->
  try List.assoc (String.lowercase_ascii s) sys_spec
  with Not_found -> raise (UnsupportedSystem s)

let string_of_system : system -> string = function
  | `Coq -> "coq"
  | `Matita -> "matita"
  | `OpenTheory -> "opentheory"
  | `Pvs -> "pvs"
  | `Lean -> "lean"
