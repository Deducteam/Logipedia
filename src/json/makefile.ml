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

    Denoting [ms] the (transitive closure of the) dependencies of
    module [md], the target is the out json file, it depends on the
    json files stemming from [ms]. *)
let json : (DkTools.Mident.t -> DkTools.entry list pp) -> DkTools.Mident.t ->
  (Key.t, Value.t) rule = fun pp_entries md ->
  let tg_of_md md = Api.Dep.get_file md |> mk_target in
  let tg = tg_of_md md in
  let md_deps =
    Deps.deps_of_md md |> DkTools.MdSet.to_seq |> List.of_seq
  in
  let md_deps = List.map (fun m -> Key.create (tg_of_md m)) md_deps in
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
  Key.create obj |> List.fold_right depends md_deps |>
  assemble print

(** [dkpth_for p] returns the Dedukti source file path of target [p] or

    @raise Not_found *)
let dkpth_for : string -> string = fun t ->
  let module E = Api.Env.Default in
  let open Filename in
  let md = E.init (!/t <.> "dk") in
  DkTools.get_file md

let make_rule_gen : (module Compile.S) -> (_, _) generator list =
  fun (module JsExp) ->
  let lift : (string -> bool) -> (string -> 'a) -> Key.t -> 'a option =
  fun filter f k ->
    match k with
    | File(p) when filter p -> Some(f p)
    | File(_)
    | Sign(_)
    | Phon(_)               -> None
  in
  let pp_entries md : DkTools.entry list pp = fun fmt ens ->
    JsExp.doc_of_entries md ens |> JsExp.print_document fmt
  in
  let module E = Api.Env.Default in
  let ext_is e s = (Filename.extension s) = e in
  let json = lift (ext_is ".json") (fun f -> json pp_entries (E.init Filename.(!/f <.> "dk"))) in
  let objrule = lift (ext_is ".dko") (fun f -> Rule.dko f) in
  let filrule = lift (ext_is ".dk") (fun f -> Rule.need (dkpth_for f)) in
  [json; objrule; filrule]
