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
  let sigrule f = load (E.init f) in
  let sysrule f = sys (mk_target f) entries_pp (E.init f) in
  let objrule f = dko_of f in
  let logic_rules =
    (* Kind of unsafe *)
    let sttfamd = B.mk_mident "sttfa" in
    [load sttfamd; objrule (Api.Dep.get_file sttfamd)]
  in
  logic_rules @
  (List.map (fun t -> [objrule t; sigrule t; sysrule t]) files |> List.flatten)

(** A basis for sttfa makefiles. *)
module Basis =
struct
  include Build_template
  let want : path -> key = fun p -> `K_file(p)
end
