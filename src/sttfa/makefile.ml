open Core
open Build.Classic
open Console
open Extras
open Build_template
open Dk

(** [mk_sysrule ~target ~entries_pp md] creates a rule that prints entries of
    module [md] with [~entries_pp] into file [~target]. *)
let mk_sysrule : target:string -> entries_pp:entry list pp -> mident ->
  (key, value) rule = fun ~target ~entries_pp md ->
  let m_creates = `Kfile(target) in
  let m_depends = [`Ksign(md)] in
  let m_action entries =
    if !log_enabled then log "[build] [%a]" pp_key m_creates;
    let ochan = open_out target in
    let ofmt = Format.formatter_of_out_channel ochan in
    match entries with
    | [`Vsign(entries)] ->
      entries_pp ofmt entries;
      close_out ochan;
      `Vfile(target, time target)
    | _                 -> assert false
  in
  {m_creates; m_depends; m_action}

(** [rules_for ~target ~entries_pp md] yields all the rules necessary to export
    module [md] to path [~target] using [~entries_pp] to print entries. *)
let rules_for : target:path -> entries_pp:entry list pp -> mident ->
  (key, value) rule list = fun ~target ~entries_pp md ->
  let sigrule = mk_sigrule md in
  let sysrule = mk_sysrule ~target ~entries_pp md in
  let logic_sign_rule = mk_sigrule (Kernel.Basic.mk_mident "sttfa") in
  (* FIXME: rule will be added several times to the list, uniqify? *)
  [logic_sign_rule; sigrule; sysrule]
