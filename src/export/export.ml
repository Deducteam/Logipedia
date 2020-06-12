(** Signature of an export system must comply with the signature
    {!val:S} defined here. *)

open Core
open Extras

(** Export system must have this signature. *)
module type S =
sig
  type ast

  val target : Systems.t
  (** Which target system to export to. *)

  val compile : Kernel.Basic.mident -> Parsers.Entry.entry list -> ast
  (** [compile md es] builds an ast out of a list of Dedukti entries
      [es] coming from module [md]. *)

  val decompile : ast -> Parsers.Entry.entry list
  (** [decompile ast] returns the list of Dedukti entries coming from
      ast [ast]. *)

  val export : ast pp
  (** [export fmt ast] exports abstract syntax tree [ast] to formatter
      [fmt] in the syntax of the system. *)
end

module GenBuildSys (E:S) : Makefile.S = struct
  open Build_template
  open Build.Classic
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

  let mk_generators : string -> (DkTools.Mident.t -> DkTools.entry list pp) ->
    (Key.t, Value.t) generator list = fun ext entries_pp ->
    let sysrule = function
      | Key.File(p) when Filename.extension p = ext ->
        let srcmd = dk_of p |> Kernel.Basic.mk_mident in
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

  let generators =
    let entries_pp md fmt ens = E.compile md ens |> E.export fmt in
    mk_generators ~.file_ext entries_pp

  let want = List.map (fun x -> Key.create @@ mk_target x)
end
