(** Rules to export dk files to json. *)
open Core
open Build.Classic
open Build_template

(** [rules_for JsExp files mk_target] results in the rules needed to export
    files [files] to json using Json exporter [JsExp] and the function
    [mk_target] such that [mk_target f] is the filepath of the target. *)
let rules_for : (module Compile.S) -> path list -> (path -> path) ->
  (_ Dk.key, _ Dk.value) rule list = fun (module JsExp) files mk_target ->
  let log_rule = Build.log_rule.logger in
  let mk_rule file =
    let tg = mk_target file in
    let md = Api.Env.Default.init file in
    let md_deps =
      let mds = Deps.deps_of_md md in
      List.map (fun m -> `Kfile(Api.Dep.get_file m |> mk_target)) mds
    in
    let json_print values =
      log_rule ~lvl:25 "target [%s]" tg;
      let entries =
        try List.find Dk.is_vsign values |> Dk.to_entries
        with Not_found -> assert false
      in
      let ochan = open_out tg in
      let ofmt = Format.formatter_of_out_channel ochan in
      JsExp.print_document ofmt (JsExp.doc_of_entries md entries);
      close_out ochan;
      `Vfile(Dk.mtime tg)
    in
    target (`Kfile(tg)) +< `Ksign(md) |>
    List.fold_right depends md_deps |>
    assemble json_print
  in
  List.map mk_rule files @
  List.map (Dk.mk_dko ~incl:(Kernel.Basic.get_path ())) files @
  List.map Dk.mk_sigrule (List.map Api.Env.Default.init files)
