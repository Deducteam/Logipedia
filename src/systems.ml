type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Hollight ]

let systems = [`Coq ; `Matita ; `Pvs ; `OpenTheory ; `Lean ; `Hollight ]

exception UnsupportedSystem of string

(** A specification is
    - an identifier,
    - a system *)
type spec = string * system

(** Association list mapping a system name to the directory where the translated
   files have been or will be dumped. *)
let artefact_path : (system * string) list ref = ref []

(** Association list mapping keys that can be used on command line to designate
    the system. *)
let sys_spec : spec list =
  [ ( "coq"       , `Coq        )
  ; ( "matita"    , `Matita     )
  ; ( "ot"        , `OpenTheory )
  ; ( "opentheory", `OpenTheory )
  ; ( "pvs"       , `Pvs        )
  ; ( "lean"      , `Lean       )
  ; ( "hollight" , `Hollight   )] |>
  List.sort (fun (s,_) (t,_) -> String.compare s t)

(** Maps system to their extension. *)
let sys_ext : (system * string) list =
  [ ( `Coq       , "v"    )
  ; ( `Matita    , "ma"   )
  ; ( `Pvs       , "pvs"  )
  ; ( `OpenTheory, "ot"   )
  ; ( `Lean      , "lean" )
  ; ( `Hollight  , "ml"   ) ]

(** [system_of_string str] returns the system associated to the string [str]. *)
let system_of_string : string -> system = fun s ->
  try List.assoc (String.lowercase_ascii s) sys_spec
  with Not_found -> raise (UnsupportedSystem s)

let string_of_system : system -> string = function
  | `Hollight -> "hollight"
  | `Coq -> "coq"
  | `Matita -> "matita"
  | `OpenTheory -> "opentheory"
  | `Pvs -> "pvs"
  | `Lean -> "lean"
