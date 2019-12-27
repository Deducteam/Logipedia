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

(** [atime p] returns the modification time of a file. *)
let atime : path -> float = fun path -> Unix.((stat path).st_atime)

(** Keys used. *)
type key =
  | K_file of path
  (** A file to create. *)
  | K_sign of mident
  (** The signature of a module. *)
  | K_cfil of path
  (** A file to check. *)

(** [key_eq k l] is equality among keys. *)
let key_eq k l = match k, l with
  | K_file(p), K_file(q)
  | K_cfil(p), K_cfil(q) -> String.equal p q
  | K_sign(m), K_sign(n) -> mident_eq m n
  | _                    -> false

(** [pp_key fmt k] prints key [k] to formatter [fmt]. *)
let pp_key : key pp = fun fmt k ->
  let out ofmt = Format.fprintf fmt ofmt in
  match k with
  | K_file(p) -> out "File(%s)" p
  | K_cfil(p) -> out "Check(%s)" p
  | K_sign(m) -> out "Load(%a)" Api.Pp.Default.print_mident m

(** [check p] sets path [p] as a to be checked target. *)
let check : path -> key = fun p -> K_cfil(p)

(** [create p] sets path [p] as a to be created file. *)
let create : path -> key = fun p -> K_file(p)

(** Values that can be requested from a build run. *)
type value =
  | V_wfil of float
  (** The modification time of a written file. *)
  | V_rfil of float
  (** The access time of a read file. *)
  | V_sign of entry list
  (** The entries of a signature. *)

let valid_stored : key -> value -> bool = fun k v -> match k, v with
  | K_file(p), V_wfil(t) -> Sys.file_exists p && t >= mtime p
  | K_cfil(p), V_rfil(t) -> Sys.file_exists p && t >= atime p
  | K_sign(_), V_sign(_) -> false
  | _                    -> invalid_arg "Build_shelf.valid_stored"

let is_vsign : value -> bool = function
  | V_sign(_) -> true
  | _         -> false

let to_entries : value -> entry list = function
  | V_sign(x) -> x
  | _         -> invalid_arg "Build_shelf.to_entries"

(** [dko_of f] creates a rule to create the Dedukti object file of [f], with. *)
let dko_of : path -> (key, value) rule = fun file ->
  let dir = Filename.dirname file in
  let out = objectify file in
  let md_deps =
    Deps.deps_of_md (Api.Env.Default.init file) |>
    List.map (fun m -> Api.Dep.get_file m |> objectify) |>
    List.map (fun x -> K_file(x))
  in
  let dkcheck _ =
    let incl = Kernel.Basic.get_path () in
    let includes = List.map ((^) "-I ") incl |> String.concat " " in
    let cmd = Format.sprintf "dkcheck -e %s -I %s %s" includes dir file in
    log_rule ~lvl:25 "%s" cmd;
    if Sys.command cmd <> 0 then log_rule ~lvl:10 "failure [%s]" cmd;
    V_wfil(mtime out)
  in
  target (K_file(out)) |>
  List.fold_right depends md_deps |>
  assemble dkcheck

(** [load md] creates a rule to load module [md] into the signature and
    compute the entries of [md]. *)
let load : mident -> (key, value) rule = fun md ->
  let file = Api.Dep.get_file md in
  let sigs = Deps.deps_of_md md |> List.map (fun x -> K_sign(x)) in
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
    V_sign(entries)
  in
  target (K_sign(md)) +< K_file(objectify file) |>
  List.fold_right depends sigs |> assemble action

(** [sys target entries_pp md] creates a rule that prints entries of
    module [md] with [entries_pp md] into file [target]. *)
let sys : path -> (mident -> entry list pp) -> mident -> (key, value) rule =
  fun tg pp_entries md ->
  let pp_entries = pp_entries md in
  let print entries =
    log_rule ~lvl:25 "printing [%s]" tg;
    let ochan = open_out tg in
    let ofmt = Format.formatter_of_out_channel ochan in
    match entries with
    | [V_sign(entries)] ->
      pp_entries ofmt entries;
      close_out ochan;
      V_wfil(mtime tg)
    | _                 -> assert false
  in
  target (K_file(tg)) +< (K_sign(md)) +> print

let json : (path -> path) -> (mident -> entry list pp) -> mident ->
  (key, value) rule =
  fun mk_target pp_entries md ->
  let tg_of_md md = Api.Dep.get_file md |> mk_target in
  let tg = tg_of_md md in
  let md_deps =
    List.map (fun m -> K_file(tg_of_md m)) (Deps.deps_of_md md)
  in
  let pp_entries = pp_entries md in
  let print values =
    log_rule ~lvl:25 "json [%s]" tg;
    let ochan = open_out tg in
    let ofmt = Format.formatter_of_out_channel ochan in
    List.find is_vsign values |> to_entries |> pp_entries ofmt;
    close_out ochan;
    V_wfil(mtime tg)
  in
  target (K_file(tg)) +< K_sign(md) |> (List.fold_right depends md_deps) |>
  assemble print

(** [check cmd pth] creates a rule to check file [pth] with command [cmd] which
    should return 0 in case of success. *)
let check_with : string -> path -> (key, value) rule = fun cmd pth ->
    let check _ =
      log_rule ~lvl:25 "%s" cmd;
      if Sys.command cmd <> 0 then log_rule ~lvl:10 "Command failure [%s]" cmd;
      V_rfil(atime pth)
    in
    target (K_cfil(pth)) +< K_file(pth) +> check
