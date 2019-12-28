(** Define some rule making facilites for export from STTfa. *)
open Core
open Extras
open Build.Classic
open Build_template

(** The rule of a system file roughly looks like, where [export] is the export
    folder, [sys] the extension of the system files, [file] the module,

    {v
    `Vfile(export/file.sys): `Vsign(entries)
         entries_pp entries
    v} *)

(** [rules_for files f_ext entries_pp] yields all the rules necessary to export
    source files [files] using [entries_pp] to print entries. The file extension
    is [f_ext]. [entries_pp] is a function such that for any module [md],
    [entries_pp md] is a usable printer for entries. *)
let rules_for : (string * string) list -> (mident -> entry list pp) ->
  (key, value) rule list =
  fun files entries_pp ->
  let module B = Kernel.Basic in
  let module E = Api.Env.Default in
  let sigrule (s,_) = load (E.init s) in
  let sysrule (s,t) = entry_printer t entries_pp (E.init s) in
  let objrule (s,_) = dko_of s in
  let logic_rules =
    let sttfamd = B.mk_mident "sttfa" in
    [load sttfamd; dko_of (Api.Dep.get_file sttfamd)]
  in
  logic_rules @
  (List.map (fun t -> [objrule t; sigrule t; sysrule t]) files |> List.flatten)

(** A basis for sttfa makefiles. *)
module Basis =
struct
  type nonrec key = key
  let key_eq = key_eq
  let pp_key = pp_key
  type nonrec value = value
  let valid_stored = valid_stored
end
