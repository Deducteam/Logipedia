(** Uses the make facility to solve dependency issues. *)

open Core
open Json
module K = Kernel
module Denv = Api.Env.Default

let theory = "dummy"
let theory_md = K.Basic.mk_mident theory

let get_mod_deps : K.Basic.mident -> K.Basic.mident list = fun _ -> assert false

module JsExp = Json.Compile.Make(Middleware.Sttfa)

(** {1 General definitions} *)

type key =
  | JsMd of K.Basic.mident
  | DkMd of K.Basic.mident

(** {1 GNU Make like behaviour} *)
(** Old style make rules. *)
type ('k, 'v) rulem = { m_creates : 'k
                      ; m_depends : 'k list
                      ; m_action : 'v list -> 'v }

let rec buildm : ('k, 'v) rulem list -> 'k -> 'v = fun rules target ->
  let rule = List.find (fun r -> r.m_creates = target) rules in
  rule.m_action (List.map (buildm rules) rule.m_depends)

let make_doc_rulem : string -> (key, Json_types.document) rulem =
  fun file ->
  let md = Denv.init file in
  let m_action _ =
    (* Argument discarded as we previously built jsons in
       [doc_of_entries]. Memoization can be done at the level of make,
       and not [doc_of_entries]. *)
    let input = open_in file in
    let entries = Parsing.Parser.Parse_channel.parse md input in
    JsExp.doc_of_entries md entries
  in
  let m_depends = List.map (fun x -> DkMd(x)) (get_mod_deps md) in
  { m_creates=JsMd(md); m_depends; m_action}

(** {1 Shake like behaviour} *)

type ('k, 'v) action =
  | Finished of 'v
  | Depends  of 'k * ('v -> ('k, 'v) action)
  (** A dependence on a key along with the way to use the value from
      this dependency. *)

(** Dynamic rules. *)
type ('k, 'v) rule = 'k * ('k, 'v) action

let rec build : ('k, 'v) rule list -> 'k -> 'v = fun rules target ->
  let rec run = function
    | Finished(v)  -> v
    | Depends(d,a) -> run (a (build rules d))
  in
  let action = snd (List.find (fun r -> (fst r) = target) rules) in
  run action

let make_doc_rule : string -> (key, Json_types.document) rule = fun file ->
  let md = Denv.init file in
  let action =
    Depends(DkMd(md), fun _ ->
        let input = open_in file in
        let entries = Parsing.Parser.Parse_channel.parse md input in
        Depends(DkMd(theory_md), fun _ ->
            Finished(JsExp.doc_of_entries md entries)))
  in
  JsMd(md), action
