(** Define some rule making facilites for export from STTfa. *)
open Core
open Extras
open Build.Classic
open Build_template

(** [rules_for files entries_pp] yields all the rules necessary to export source
    files (first elements of [files]) to targets (second elements of) [files]
    using [entries_pp] to print entries. [entries_pp] is a function such that
    for any module [md], [entries_pp md] is a usable printer for entries. *)
let rules_for : (string * string) list ->
  (DkTools.Mident.t -> DkTools.entry list pp) -> (Key.t, Value.t) rule list =
  fun files entries_pp ->
  let module B = Kernel.Basic in
  let module E = Api.Env.Default in
  let sigrule (s,_) = Rule.load (E.init s) in
  let sysrule (s,t) = Rule.entry_printer t entries_pp (E.init s) in
  let objrule (s,_) = Rule.dko s in
  let filrule (s,_) = Rule.need s in
  let logic_rules =
    let sttfamd = B.mk_mident "sttfa" in
    let sttfafile = DkTools.get_file sttfamd in
    Rule.[need sttfafile; load sttfamd; dko sttfafile]
  in
  logic_rules @
  (List.map (fun t -> [filrule t; objrule t; sigrule t; sysrule t]) files |>
   List.flatten)

(** A basis for sttfa makefiles. *)
module Basis =
struct
  type nonrec key = Key.t
  let key_eq = Key.eq
  let pp_key = Key.pp
  type nonrec value = Value.t
  let valid_stored = valid_stored
end
