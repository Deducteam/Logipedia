(** Rules to export dk files to json. *)
open Core
open Extras
open Build.Classic
open Build_template

(** [is_vsign v] is true iff [v] is a signature. *)
let is_vsign : [> `Vsign of entry list] -> bool = function
  | `Vsign(_) -> true | _ -> false

(** [to_entries v] returns the entries of value [v]. *)
let to_entries : [> `Vsign of entry list] -> entry list = function
  | `Vsign(x) -> x | _ -> assert false

let key_eq : _ eq = fun k l ->
  match k, l with
  | `Ksign(s), `Ksign(t) -> Sign.key_eq s t
  | `Kfile(p), `Kfile(q) -> Dkob.key_eq p q
  | _                    -> false

let pp_key : _ pp = fun fmt k ->
  match k with
  | `Ksign(s) -> Sign.pp_key fmt s
  | `Kfile(p) -> Dkob.pp_key fmt p

let valid_stored : _ -> _ -> bool = fun k v ->
  match k, v with
  | `Ksign(s), `Vsign(e) -> Sign.valid_stored s e
  | `Kfile(p), `Vfile(t) -> Dkob.valid_stored p t
  | _                    -> false

let want : path -> _ = fun p -> `Kfile(p)

(** [rules_for JsExp files mk_target] results in the rules needed to export
    files [files] to json using Json exporter [JsExp] and the function
    [mk_target] such that [mk_target f] is the filepath of the target. *)
let rules_for : (module Compile.S) -> path list -> (path -> path) ->
  (_, _) rule list = fun (module JsExp) files mk_target ->
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
        try List.find is_vsign values |> to_entries
        with Not_found -> assert false
      in
      let ochan = open_out tg in
      let ofmt = Format.formatter_of_out_channel ochan in
      JsExp.print_document ofmt (JsExp.doc_of_entries md entries);
      close_out ochan;
      `Vfile(mtime tg)
    in
    target (`Kfile(tg)) +< `Ksign(md) |>
    List.fold_right depends md_deps |>
    assemble json_print
  in
  List.map mk_rule files @
  List.map (Dkob.mk_dko ~incl:(Kernel.Basic.get_path ())) files @
  List.map Sign.mk_sigrule (List.map Api.Env.Default.init files)
