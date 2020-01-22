(** Define some rule making facilites *)
open Core
open Extras
open Build.Classic
open Build_template

let mk_generators : string -> (DkTools.Mident.t -> DkTools.entry list pp) ->
  (Key.t, Value.t) generator list = fun ext entries_pp ->
  let sysrule = function
    | Key.File(p) when Filename.extension p = ext ->
      let srcmd = dk_of p |> Api.Env.Default.init in
      Some(Rule.entry_printer p entries_pp srcmd)
    | _                                           -> None
  in
  let objrule = function
    | Key.File(p) when Filename.extension p = ".dko" -> Some(Rule.dko p)
    | _                                              -> None
  in
  let filrule = function
    | Key.File(p) when Filename.extension p = ".dk" -> Some(Rule.need p)
    | _                                             -> None
  in
  [sysrule; objrule; filrule]

(** Build rules for the system. Provides the rules, keys and values to use the
    build system {!module:Build.Classic}. *)
module type MAKEFILE =
sig
  open Build.Classic

  type key
  (** Keys or targets. *)

  type value
  (** Values are either the created files or the loaded signatures. *)

  val key_eq : key eq
  (** Equality on keys. *)

  val pp_key : key pp
  (** [pp_key fmt k] prints key [k] to formatter [fmt]. *)

  val valid_stored : key -> value -> bool
  (** [valid_stored k v] returns whether value [v] stored in the database is a
      valid value of key [k]. *)

  val want : string list -> key list
  (** [want p] creates targets out of paths [p]. Used to declare initial
      targets. *)

  val rules : (key, value) rule list
  (** Static rules. *)

  val generators : (key, value) generator list
  (** Dynamic rules generators used *)
end

module Make(E:Export.S) : MAKEFILE =
struct
  open Build_template
  open Filename
  open Console

  type key = Key.t
  type value = Value.t

  let key_eq = Key.eq
  let pp_key = Key.pp
  let valid_stored = valid_stored
  let rules = []

  let file_ext = List.assoc E.target Systems.exts

  let mk_target f = (Option.get !Cli.outdir) </> !/f <.> file_ext

  let generators =
    let entries_pp md fmt ens = E.compile md ens |> E.export fmt in
    mk_generators ~.file_ext entries_pp

  let want = List.map (fun x -> Key.create @@ mk_target x)
end
