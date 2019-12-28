(** Rules to export dk files to json. *)
open Core
open Extras
open Build.Classic

include Build_template

let want : path -> key = create

let json : (path -> path) -> (mident -> entry list pp) -> mident ->
  (key, value) rule = fun mk_target pp_entries md ->
  let tg_of_md md = Api.Dep.get_file md |> mk_target in
  let tg = tg_of_md md in
  let md_deps =
    List.map (fun m -> `K_file(tg_of_md m)) (Deps.deps_of_md md)
  in
  let pp_entries = pp_entries md in
  let print values =
    log_rule ~lvl:25 "json [%s]" tg;
    let ochan = open_out tg in
    let ofmt = Format.formatter_of_out_channel ochan in
    List.find is_vsign values |> to_entries |> pp_entries ofmt;
    close_out ochan;
    `V_wfil(mtime tg)
  in
  target (`K_file(tg)) +< `K_sign(md) |> (List.fold_right depends md_deps) |>
  assemble print

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

