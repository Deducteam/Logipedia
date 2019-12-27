(** Some rule making facilities. *)
open Extras
open Console
open Build.Classic

(** Some shorthands. *)
type mident = Kernel.Basic.mident
let mident_eq : mident eq = Kernel.Basic.mident_eq
type entry = Parsing.Entry.entry

(** Type of a filepath. *)
type path = string

let log_rule = Build.log_rule.logger

(** [objectify pth] transforms [f.dk] in [f.dko]. If [file] is not of the form
    [f.dk],
    @raise Invalid_argument *)
let objectify : path -> path = fun file ->
  if Filename.extension file <> ".dk" then invalid_arg "StdBuild.mk_dko";
  file ^ "o"

(** [mtime p] returns the modification time of a file. *)
let mtime : path -> float = fun path -> Unix.((stat path).st_mtime)

(** Provide functions to create rules for dko files. *)
module Dkob =
struct
  type key = path
  (** Path to a dko. *)

  type value = float
  (** Entries in the file. *)

  (** [pp_key fmt key] prints key [key] to formatter [fmt]. *)
  let pp_key : key pp = fun fmt pth -> Format.fprintf fmt "File(%s)" pth

  (** [key_eq k l] returns whether keys [k] and [l] are equal. *)
  let key_eq : key eq = String.equal

  (** [valid_stored k v] returns true *)
  let valid_stored : key -> value -> bool = fun p t ->
    Sys.file_exists p && t >= (mtime p)

  (** [mk_dko ?incl f] creates a rule to create the Dedukti object file of
      [f], with include path [?incl]. *)
  let mk_dko : ?incl:(path list) -> path -> (_, _) rule =
    fun ?(incl=[]) file ->
    let log_rule = Build.log_rule.logger in
    let dir = Filename.dirname file in
    let out = objectify file in
    let md_deps =
      Deps.deps_of_md (Api.Env.Default.init file) |>
      List.map (fun m -> Api.Dep.get_file m |> objectify) |>
      List.map (fun x -> `Kfile(x))
    in
    let dkcheck _ =
      let includes = List.map ((^) "-I ") incl |> String.concat " " in
      let cmd = Format.sprintf "dkcheck -e %s -I %s %s" includes dir file in
      log_rule ~lvl:25 "%s" cmd;
      if Sys.command cmd <> 0 then log_rule ~lvl:10 "failure";
      `Vfile(mtime out)
    in
    target (`Kfile(out)) |>
    List.fold_right depends md_deps |>
    assemble dkcheck
end

(** Rules to load files into signatures and get the entries. *)
module Sign =
struct
  type key = mident
  type value = entry list

  (** [pp_key fmt key] prints key [key] to formatter [fmt]. *)
  let pp_key : key pp = fun fmt m ->
    Format.fprintf fmt "Signature of %a" Api.Pp.Default.print_mident m

  (** [key_eq k l] returns whether keys [k] and [l] are equal. *)
  let key_eq : key eq = fun m n -> mident_eq m n

  let valid_stored : key -> value -> bool = fun _ _ -> false

  (** [mk_sigrule md] creates a rule to load module [md] into the signature and
      compute the entries of [md]. *)
  let mk_sigrule : mident -> (_, _) rule = fun md ->
    let log_rule = Build.log_rule.logger in
    let file = Api.Dep.get_file md in
    let sigs = Deps.deps_of_md md |> List.map (fun x -> `Ksign(x)) in
    let action _ =
      log_rule ~lvl:25 "loading %s" file;
      let inchan = open_in file in
      let entries = Parsing.Parser.Parse_channel.parse md inchan in
      close_in inchan;
      let declare e =
        let open Parsing.Entry in
        let module Denv = Api.Env.Default in
        let module S = Kernel.Signature in
        let module E = Api.Env in
        try
          begin match e with
            | Decl(lc,id,st,ty)         -> Denv.declare lc id st ty
            | Def(lc,id,op,Some(ty),te) -> Denv.define lc id op te (Some ty)
            | _                         -> ()
          end
        with E.EnvError(_,_,EnvErrorSignature(S.AlreadyDefinedSymbol(_))) -> ()
      in
      List.iter declare entries;
      `Vsign(entries)
    in
    target (`Ksign(md)) +< `Kfile(objectify file) |>
    List.fold_right depends sigs |>
    assemble action
end
