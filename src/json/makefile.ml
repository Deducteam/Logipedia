(** Rules to export dk files to json. *)
open Core
open Extras
open Console
open Build.Classic
open Build_template

let pp_key = Key.pp
let valid_stored = valid_stored
let key_eq = Key.eq

let mk_target f =
  let open Filename in
  (Option.get !Cli.outdir) </> !/f <.> "json"

let want : string list -> Key.t list =
  List.map (fun x -> Key.create (mk_target x))

(** [json f md] creates a rule that prints entries of module [md] to a
    file with pretty printer [pp_entries] obtained from [f md].

    Denoting [ms] the dependencies of module [md], the target is the out json
    file, it depends on the json files stemming from [ms]. *)
let json : (DkTools.mident -> DkTools.entry list pp) -> DkTools.mident ->
  (Key.t, Value.t) rule = fun pp_entries md ->
  let tg_of_md md = Api.Dep.get_file md |> mk_target in
  let tg = tg_of_md md in
  let md_deps =
    List.map (fun m -> Key.create (tg_of_md m)) (Deps.deps_of_md md)
  in
  let pp_entries = pp_entries md in
  let print _ =
    log_rule ~lvl:3 "json [%s]" tg;
    let ochan = open_out tg in
    let ofmt = Format.formatter_of_out_channel ochan in
    let entries =
      let inchan = open_in (Api.Dep.get_file md) in
      let r = Parsing.Parser.Parse_channel.parse md inchan in
      close_in inchan;
      r
    in
    pp_entries ofmt entries;
    close_out ochan;
    Value.written tg
  in
  let obj = objectify (DkTools.get_file md) in
  target (Key.create tg) +<
  Key.create obj |> (List.fold_right depends md_deps) |>
  assemble print

(** [rules_for encoding JsExp files] results in the rules needed to
    export files [files] to json using Json exporter [JsExp] and
    dedukti encoding [encoding]. *)
let rules_for : DkTools.mident list -> (module Compile.S) -> string list ->
  (Key.t, Value.t) rule list = fun encoding (module JsExp) files ->
  let pp_entries md : DkTools.entry list pp = fun fmt ens ->
    JsExp.doc_of_entries md ens |> JsExp.print_document fmt
  in
  let module E = Api.Env.Default in
  let json f = json pp_entries (E.init f) in
  let objrule f = Rule.dko f in
  let filrule f = Rule.need f in
  (* Remove all modules related to encoding from the list. *)
  let clear_encoding l =
    let open List in
    fold_right (fun e r -> remove_eq DkTools.mident_eq e r) encoding l
  in
  (* Create the rules to create json files of dependencies, as they
     may be needed (to at list check that the json file is already
     built) *)
  let deps =
    (* FIXME Using sets would be much more efficient. *)
    List.map E.init files |> List.map Deps.deps_of_md |> List.flatten |>
    List.uniq_eq DkTools.mident_eq |>
    clear_encoding |>
    List.map Api.Dep.get_file
  in
  List.map (fun t -> [filrule t; objrule t; json t]) (files @ deps) |>
  List.flatten
