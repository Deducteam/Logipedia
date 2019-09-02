type system = [`Coq | `Matita | `Pvs | `OpenTheory | `Lean | `Json ]

let systems = [`Coq ; `Matita ; `Pvs ; `OpenTheory ; `Lean ; `Json ]

let system_of_string : string -> system = fun s ->
  if s = "coq" then
    `Coq
  else if s = "matita" then
     `Matita
  else if s = "ot" || s = "opentheory" then
    `OpenTheory
  else if s = "pvs" then
    `Pvs
  else if s = "lean" then
    `Lean
  else if s = "json" then
    `Json
  else
    failwith (Format.sprintf "%s is not among the supported systems@." s)

let string_of_system : system -> string = function
  | `Coq -> "coq"
  | `Matita -> "matita"
  | `OpenTheory -> "opentheory"
  | `Pvs -> "pvs"
  | `Lean -> "lean"
  | `Json -> "json"
