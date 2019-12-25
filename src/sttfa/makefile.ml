(** Define some rule making facilites for export from STTfa. *)
open Core
open Build.Classic
open Console
open Extras
open Build_template
open Dk

let log_rule = Build.log_rule.logger

(** The rule of a system file roughly looks like, where [export] is the export
    folder, [sys] the extension of the system files, [file] the module,

    {v
    `Vfile(export/file.sys): `Vsign(entries)
         entries_pp entries
    v} *)

(** [mk_sysrule target entries_pp md] creates a rule that prints entries of
    module [md] with [entries_pp md] into file [target]. *)
let mk_sysrule : path -> (mident -> entry list pp) -> mident ->
  (key, value) rule = fun target pp_entries md ->
  let m_creates = `Kfile(target) in
  let m_depends = [`Ksign(md)] in
  let pp_entries = pp_entries md in
  let m_action entries =
    log_rule ~lvl:25 "target [%a]" pp_key m_creates;
    let ochan = open_out target in
    let ofmt = Format.formatter_of_out_channel ochan in
    match entries with
    | [`Vsign(entries)] ->
      pp_entries ofmt entries;
      close_out ochan;
      `Vfile(target, time target)
    | _                 -> assert false
  in
  {m_creates; m_depends; m_action}

(** [rules_for files mk_target entries_pp] yields all the rules necessary to
    export source files [files] using [entries_pp] to print entries. The
    function [mk_target] transforms a file of [files] into the target filepath.
    [entries_pp] is a function such that for any module [md], [entries_pp md] is
    a usable printer for entries. *)
let rules_for : path list -> (path -> path) -> (mident -> entry list pp) ->
  (key, value) rule list =
  fun files mk_target entries_pp ->
  let module B = Kernel.Basic in
  let module E = Api.Env.Default in
  let sigrule f = mk_sigrule (E.init f) in
  let sysrule f = mk_sysrule (mk_target f) entries_pp (E.init f) in
  let objrule f = mk_dko ~incl:(B.get_path ()) f in
  let logic_rules =
    (* Kind of unsafe *)
    let sttfamd = B.mk_mident "sttfa" in
    [mk_sigrule sttfamd; objrule (Api.Dep.get_file sttfamd)]
  in
  logic_rules @
  (List.map (fun t -> [objrule t; sigrule t; sysrule t]) files |> List.flatten)
