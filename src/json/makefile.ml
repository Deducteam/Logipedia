(** Rules to export dk files to json. *)
open Core
open Extras
open Build.Classic
open Build_template

let pp_key = pp_key
let valid_stored = valid_stored
let key_eq = key_eq

let mk_target f =
  let open Filename in
  (Option.get !outdir) </> !/f <.> "json"

let want : string list -> key list =
  List.map (fun x -> create (mk_target x))

let json : (mident -> entry list pp) -> mident -> (key, value) rule =
  fun pp_entries md ->
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
let rules_for : (module Compile.S) -> string list ->
  (key, value) rule list = fun (module JsExp) files ->
  let pp_entries md : entry list pp = fun fmt ens ->
    JsExp.doc_of_entries md ens |> JsExp.print_document fmt
  in
  let module E = Api.Env.Default in
  let loadrule f = load (E.init f) in
  let json f = json pp_entries (E.init f) in
  let objrule f = dko_of f in
  List.map (fun t -> [objrule t; loadrule t; json t]) files |> List.flatten

