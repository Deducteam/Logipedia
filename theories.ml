(** The input theory **)


type theory = [`STTFA | `HOL]

let theory_of_string : string -> theory = fun s ->
  if s = "sttfa" then
    `STTFA
  else if s = "hol" then
    `HOL
  else
    failwith (Format.sprintf "%s is not among the supported theories@." s)
