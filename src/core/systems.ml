type t = Coq | Matita | Pvs | OpenTheory | Lean | Hollight | Latex

let systems = [Coq ; Matita ; Pvs ; OpenTheory ; Lean ; Hollight ; Latex]

exception UnsupportedSystem of string

(** A specification is
    - an identifier,
    - a system *)
type spec = string * t

(** Association list mapping a system name to the directory where the translated
   files have been or will be dumped. *)
let artefact_path : (t * string) list ref = ref []

(** Association list mapping keys that can be used on command line to designate
    the system. *)
let spec : spec list =
  [ ( "coq"       , Coq        )
  ; ( "matita"    , Matita     )
  ; ( "ot"        , OpenTheory )
  ; ( "opentheory", OpenTheory )
  ; ( "pvs"       , Pvs        )
  ; ( "lean"      , Lean       )
  ; ( "hollight"  , Hollight   )
  ; ( "latex"     , Latex      )] |>
  List.sort (fun (s,_) (t,_) -> String.compare s t)

(** Maps system to their extension. *)
let exts : (t * string) list =
  [ ( Coq       , "v"    )
  ; ( Matita    , "ma"   )
  ; ( Pvs       , "pvs"  )
  ; ( OpenTheory, "ot"   )
  ; ( Lean      , "lean" )
  ; ( Hollight  , "ml"   )
  ; ( Latex     , "tex"  ) ]

(** [of_string str] returns the system associated to the string [str]. *)
let of_string : string -> t = fun s ->
  try List.assoc (String.lowercase_ascii s) spec
  with Not_found -> raise (UnsupportedSystem s)

let to_string : t -> string = function
  | Hollight   -> "hollight"
  | Coq        -> "coq"
  | Matita     -> "matita"
  | OpenTheory -> "opentheory"
  | Pvs        -> "pvs"
  | Lean       -> "lean"
  | Latex      -> "latex"
