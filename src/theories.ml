type theory = [`Sttfa | `Hol]

let theory_of_string : string -> theory = fun s ->
  if s = "sttfa" then
    `Sttfa
  else if s = "hol" then
    `Hol
  else
    failwith (Format.sprintf "the theory %s is not known@." s)

let string_of_theory : theory -> string = function
  | `Sttfa -> "sttfa"
  | `Hol -> "hol"

type edge =
  {
    input:theory;
    output:theory;
    script:string; (** path to the script *)
  }

let dir : string = "interoperability"

let hol_sttfa : edge =
  {
    input=`Hol;
    output=`Sttfa;
    script=Filename.concat dir "hol_sttfa/Makefile"
  }

let graph : edge list =
  [hol_sttfa]
