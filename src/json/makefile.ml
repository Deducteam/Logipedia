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

(** [json f p] creates a rule creating file [q.json] where [p = q.json]
    containing entries that are in [q.dk] file with pretty printer [pp_entries]
    obtained from [f md].

    FIXME no transitive closure anymore
    Denoting [ms] the (transitive closure of the) dependencies of
    module [md], the target is the out json file, it depends on the
    json files stemming from [ms]. *)
let json : (DkTools.Mident.t -> DkTools.entry list pp) -> string ->
  (Key.t, Value.t) rule = fun pp_entries tg ->
  let module E = Api.Env.Default in
  let md = E.init Filename.(dirname tg </> !/tg <.> "dk") in
  let md_deps =
    Deps.deps_of_md md |> DkTools.MdSet.to_seq |> List.of_seq
  in
  let tg_of_md m =
    let open Filename in
    let p = DkTools.get_file m in
    Key.create ((Option.get !Cli.outdir) </> !/p <.> "json")
  in
  let md_deps = List.map tg_of_md md_deps in
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

let mk_generators : (module Compile.S) -> (Key.t, Value.t) generator list =
  fun (module JsExp) ->
  (** [lift filter f k] lifts function [f] to operate on {!type:Key.t} with
      [f k] returns [None] or [Some(v)] if [filter k] is true. *)
  let lift : (string -> bool) -> (string -> 'a) -> Key.t -> 'a option =
  fun filter f k -> match k with
    | File(p) when filter p -> Some(f p)
    | File(_)
    | Phon(_)               -> None
  in
  let pp_entries md : DkTools.entry list pp = fun fmt ens ->
    JsExp.doc_of_entries md ens |> JsExp.print_document fmt
  in
  let ext_is e s = (Filename.extension s) = e in
  let json = lift (ext_is ".json") (json pp_entries) in
  let objrule = lift (ext_is ".dko") (Rule.dko ~opt:(!Cli.dkopts)) in
  let filrule = lift (ext_is ".dk") Rule.need in
  [json; objrule; filrule]
