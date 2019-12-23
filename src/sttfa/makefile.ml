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
let mk_sysrule : string -> (mident -> entry list pp) -> mident ->
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

(** [rules_for target entries_pp] yields all the rules necessary to create
    targets in [targets] using [entries_pp] to print entries. An element of
    [target] is a tuple [(p,m)] where [p] is the filepath of the target and [m]
    the exported module identifier. [entries_pp] is a function such that for any
    module [md], [entries_pp md] is a usable printer for entries. *)
let rules_for : ((path * mident) list) -> (mident -> entry list pp) ->
  (key, value) rule list =
  fun target entries_pp ->
  let sigrule md = mk_sigrule md in
  let sysrule md target = mk_sysrule target entries_pp md in
  let logic_rule = mk_sigrule (Kernel.Basic.mk_mident "sttfa") in
  logic_rule ::
  (List.map (fun (tg, md) -> [sigrule md; sysrule md tg]) target |> List.flatten)
