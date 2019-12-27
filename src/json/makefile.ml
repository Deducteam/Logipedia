(** Rules to export dk files to json. *)
open Core
open Extras
open Build.Classic

include Build_template

let want : path -> key = create

(** [rules_for JsExp files mk_target] results in the rules needed to export
    files [files] to json using Json exporter [JsExp] and the function
    [mk_target] such that [mk_target f] is the filepath of the target. *)
let rules_for : (module Compile.S) -> path list -> (path -> path) ->
  (key, value) rule list = fun (module JsExp) files mk_target ->
  let pp_entries md : entry list pp = fun fmt ens ->
    JsExp.doc_of_entries md ens |> JsExp.print_document fmt
  in
  let module E = Api.Env.Default in
  let loadrule f = load (E.init f) in
  let json f = json mk_target pp_entries (E.init f) in
  let objrule f = dko_of f in
  List.map (fun t -> [objrule t; loadrule t; json t]) files |> List.flatten

