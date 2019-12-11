(** Uses the make facility to solve dependency issues. *)

open Core
open Json
module K = Kernel
module Denv = Api.Env.Default

let theory = "dummy"
let theory_md = K.Basic.mk_mident theory

module JsExp = Json.Compile.Make(Middleware.Sttfa)

type ('k, 'v) action =
  | Finished of 'v
  | Depends  of 'k * ('v -> ('k, 'v) action)
  (** A dependence on a key along with the way to use the value from
      this dependency. *)

type ('k, 'v) rule = 'k * ('k, 'v) action

let rec build : ('k, 'v) rule list -> 'k -> 'v = fun rules target ->
  let rec run = function
    | Finished(v)  -> v
    | Depends(d,a) -> run (a (build rules d))
  in
  let action = snd (List.find (fun r -> (fst r) = target) rules) in
  run action

let get_mod_deps : K.Basic.mident -> K.Basic.mident list = fun _ -> assert false

type key =
  | JsMd of K.Basic.mident
  | DkMd of K.Basic.mident

(** Rule to build the theory. *)
let theory_rule : (key, Json_types.document) rule =
  let input = open_in theory in
  let md = Denv.init theory in
  let entries = Parsing.Parser.Parse_channel.parse md input in
  DkMd(theory_md), Finished(JsExp.doc_of_entries md entries)

(** [compile_dkmod md] returns the action to compile the module [md]
    including the dependency on the theory file. *)
let compile_dkmod : string -> ('k, 'v) action = fun file ->
  Depends(DkMd(theory_md), fun _ ->
      let input = open_in file in
      let md = Denv.init file in
      let entries = Parsing.Parser.Parse_channel.parse md input in
      Finished(JsExp.doc_of_entries md entries))

let rule_of_dk_mod : string -> (key, Json_types.document) rule list =
  fun file ->
  let md = Denv.init file in
  [ ( JsMd(md)
    , Depends(DkMd(md), fun v -> Finished(v)) )
  ; ( DkMd(md)
    , compile_dkmod file ) ]
