type middleware = [ `Sttfa | `Cic ]

let middlewares = [`Sttfa; `Cic]

type spec = string * (module Middleware.S)

let mid_spec : spec list =
  [ ( "sttfa", (module Middleware_sttfa.Sttfa) )
  ; ( "cic"  , (module Middleware_cic.Cic    ) ) ]

(** [mid_of_string s] returns the middleware associated to string [s]. *)
let mid_of_string : string -> (module Middleware.S) = fun s ->
  try List.assoc (String.lowercase_ascii s) mid_spec
  with Not_found -> (module Middleware.Dummy)
