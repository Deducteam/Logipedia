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

(** Provides some standard functions for Dedukti related operations and file
    manipulatons. *)
module Dk =
struct
  (** Keys are either files or signatures. *)
  type 'a key = 'a constraint 'a =
    [> `Kfile of path
    (** A filepath. *)
    | `Ksign of mident
      (** The signature of a module. *) ]

  (** Values are either the created files or the loaded signatures. *)
  type 'a value = 'a constraint 'a =
    [> `Vfile of path * float
    (** A filepath along its last modification time (used to verify validity of
        a file from the database). *)
    | `Vsign of entry list
      (** Signature and the entries that are in it. *) ]

  (** [pp_key fmt key] prints key [key] to formatter [fmt]. *)
  let pp_key : _ key pp = fun fmt k ->
    let out f = Format.fprintf fmt f in
    match k with
    | `Kfile(pth) -> out "`Kfile(%s)" pth
    | `Ksign(md)  -> out "`Ksign(%a)" Api.Pp.Default.print_mident md
    | _           -> invalid_arg "Build_template.Dk.pp_key"

  (** [key_eq k l] returns whether keys [k] and [l] are equal. *)
  let key_eq : _ key eq = fun k l ->
    match k, l with
    | `Kfile(p), `Kfile(q) -> String.equal p q
    | `Ksign(m), `Ksign(n) -> mident_eq m n
    | _                    -> false

  (** [want pth] creates the key corresponding to filepath [path]. *)
  let want : path -> _ key = fun pth ->
    `Kfile(pth)

  (** [mtime p] returns the modification time of a file. *)
  let mtime : path -> float = fun path -> Unix.((stat path).st_mtime)

  (** [valid_stored k v] returns true *)
  let valid_stored : _ key -> _ value -> bool =
    fun k v -> match k, v with
    | `Kfile(p), `Vfile(_,t) -> Sys.file_exists p && t >= (mtime p)
    | `Ksign(_), `Vsign(_)   -> false
    (* Rebuild avoided by dkos for the moment *)
    | _                      -> false

  (** [is_vsign v] returns whether value [v] is a signature. *)
  let is_vsign : _ value -> bool = function
    | `Vsign(_) -> true
    | _         -> false

  (** [to_entries v] returns the list of entries associated to value [v] or
      @raise Invalid_argument *)
  let to_entries : _ value -> entry list = function
    | `Vsign(x) -> x
    | _         -> invalid_arg "Dk.to_entries"

  (** [objectify pth] transforms [f.dk] in [f.dko]. If [file] is not of the form
      [f.dk],
      @raise Invalid_argument *)
  let objectify : path -> path = fun file ->
    if Filename.extension file <> ".dk" then invalid_arg "Dk.mk_dko";
    file ^ "o"

  (** [mk_sigrule md] creates a rule to load module [md] into the signature and
      compute the entries of [md]. *)
  let mk_sigrule : mident -> (_ key, _ value) rule = fun md ->
    let log_rule = Build.log_rule.logger in
    let file = Api.Dep.get_file md in
    let sigs = Deps.deps_of_md md |> List.map (fun x -> `Ksign(x)) in
    let action : _ value list -> _ value = fun _ ->
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
    (target (`Ksign(md))) +< (`Kfile(objectify file)) |>
    List.fold_right depends sigs |>
    assemble action

  (** [mk_dko ?incl f] creates a rule to create the Dedukti object file of
      [f], with include path [?incl]. *)
  let mk_dko : ?incl:(path list) -> path -> (_ key, _ value) rule =
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
      ignore @@ Sys.command cmd;
      `Vfile(out, mtime out)
    in
    target (`Kfile(out)) |>
    List.fold_right depends md_deps |>
    assemble dkcheck
end
