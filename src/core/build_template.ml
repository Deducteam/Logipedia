(** This module defines all that is necessary to use the {!module:Build}
    module, that is, the keys, the values and some functions to create rules. *)
open Extras
open Console
open Build.Classic

open DkTools

(** A logger to be used in rules. *)
let log_rule = Build.log_rule.logger

(** [objectify pth] transforms [f.dk] in [f.dko]. If [file] is not of the form
    [f.dk],
    @raise Invalid_argument *)
let objectify : string -> string = fun file ->
  if Filename.extension file <> ".dk" then invalid_arg "objectify";
  file ^ "o"

(** [run0 cmd] runs command [cmd] and logs an error if the result is not
    zero. *)
let run0 : string -> unit -> unit = fun cmd () ->
  if Sys.command cmd <> 0 then log_rule ~lvl:2 (red "command failed [%s]") cmd

(** Definition of keys used and helper functions. *)
module Key =
struct
  (** Keys used. *)
  type t =
    | File of string
    (** A file to create. *)
    | Sign of mident
    (** The signature of a module. *)
    | Chck of string
    (** A file to check. *)
    | Phon of string
    (** A phony rule with a name. *)

  (** [eq k l] is equality among keys. *)
  let eq k l = match k, l with
    | File(p), File(q)
    | Phon(p), Phon(q)
    | Chck(p), Chck(q) -> String.equal p q
    | Sign(m), Sign(n) -> mident_eq m n
    | _                -> false

  (** [pp fmt k] prints key [k] to formatter [fmt]. *)
  let pp : t pp = fun fmt k ->
    let out ofmt = Format.fprintf fmt ofmt in
    match k with
    | File(p) -> out "File(%s)" p
    | Chck(p) -> out "Chck(%s)" p
    | Sign(m) -> out "Load(%a)" pp_mident m
    | Phon(n) -> out "Phon(%s)" n

  (** [check f] asks to check file [f]. *)
  let check : string -> t = fun p -> Chck(p)

  (** [exists p] requires the existence of file [p]. *)
  let exists = check

  (** [create f] requires the creation of file [f]. *)
  let create : string -> t = fun p -> File(p)

  (** [load m] requires to load module [m] into the signature. *)
  let load : mident -> t = fun md -> Sign(md)

  (** [fake n] requires the phony rule of name [n]. *)
  let fake : string -> t = fun name -> Phon(name)
end

(** Definitions of values and helper functions. *)
module Value =
struct
  (** Values that can be requested from a build run. *)
  type t =
    | Wfil of float
    (** The modification time of a written file. The 'intuitive' value, the
        content of the file, is already saved on the filesystem, so we don't keep
        it. *)
    | Rfil of float
    (** The access time of a read file. *)
    | Sign of entry list
    (** The content of a signature, that is, the entries. *)
    | Phon of unit
    (** Result of a phony rule. *)

  (** [checked pth] sets filepath [pth] as checked. *)
  let written : string -> t = fun pth -> Wfil(mtime pth)

  (** [loaded e] returns the value containing entries [e]. *)
  let loaded : entry list -> t = fun ens -> Sign(ens)

  (** [checked pth] sets filepath [pth] as checked. *)
  let checked : string -> t = fun pth -> Rfil(atime pth)

  (** [faked ()] returns the value for a phony target. *)
  let faked : unit -> t = fun () -> Phon(())

  let is_vsign : t -> bool = function
    | Sign(_) -> true
    | _       -> false

  let to_entries : t -> entry list = function
    | Sign(x) -> x
    | _       -> invalid_arg "to_entries"

end

(** [valid_store key value] tells whether value [value] computed from key [key]
    is up to date. *)
let valid_stored : Key.t -> Value.t -> bool = fun k v ->
  let module K = Key in let module V = Value in
  match k, v with
  | K.File(p), V.Wfil(t) -> Sys.file_exists p && t >= mtime p
  | K.File(p), V.Rfil(t)
  | K.Chck(p), V.Rfil(t) -> Sys.file_exists p && t >= atime p
  | K.Sign(_), V.Sign(_)
  | K.Phon(_), V.Phon(_) -> false
  | _                    -> invalid_arg "valid_stored"

(** Generation of rules. *)
module Rule =
struct
  (** [need pth] creates a rule that verifies if file [pth] exists. *)
  let need : string -> (Key.t, Value.t) rule = fun pth ->
    let exists _ =
      if not (Sys.file_exists pth) then exit_with "missing %s" pth else
      Value.checked pth
    in
    target (Key.create pth) +> exists

  (** [dko f] creates a rule to create the Dedukti object file of [f]. *)
  let dko : string -> (Key.t, Value.t) rule = fun file ->
    let dir = Filename.dirname file in
    let out = objectify file in
    let o_deps =
      Deps.deps_of_md (init file) |>
      List.map (fun m -> get_file m |> objectify) |>
      List.map Key.create
    in
    let dkcheck _ =
      let incl = get_path () in
      let includes = List.map ((^) "-I ") incl |> String.concat " " in
      let cmd =
        Format.sprintf "dkcheck -e %s -I %s %s 2> /dev/null"
          includes dir file in
      log_rule ~lvl:3 "%s" cmd;
      run0 cmd ();
      Value.written out
    in
    target (Key.create out) +< (Key.create file) |>
    List.fold_right depends o_deps |>
    assemble dkcheck

  (** [load md] creates a rule to load module [md] into the signature and
      compute the entries of [md]. *)
  let load : mident -> (Key.t, Value.t) rule = fun md ->
    let file = get_file md in
    let sigs = Deps.deps_of_md md |> List.map Key.load in
    let action _ =
      log_rule ~lvl:3 "loading %s" file;
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
      Value.loaded entries
    in
    target (Key.load md) +< Key.create (objectify file) |>
    List.fold_right depends sigs |> assemble action

  (** [entry_printer target entries_pp md] creates a rule that prints entries of
      module [md] with [entries_pp md] into file [target]. The target depends on
      the signature of the module. *)
  let entry_printer : string -> (mident -> entry list pp) -> mident ->
    (Key.t, Value.t) rule = fun tg pp_entries md ->
    let pp_entries = pp_entries md in
    let print entries =
      log_rule ~lvl:3 "printing [%s]" tg;
      let ochan = open_out tg in
      let ofmt = Format.formatter_of_out_channel ochan in
      match entries with
      | [x] ->
        Value.to_entries x |> pp_entries ofmt;
        close_out ochan;
        Value.written tg
      | _                  -> assert false
    in
    target (Key.create tg) +< (Key.load md) +> print

  (** [check cmd pth] creates a rule to check file [pth] with command
      [cmd] which should return 0 in case of success. *)
  let check : string -> string -> (Key.t, Value.t) rule = fun cmd pth ->
    let check _ =
      log_rule ~lvl:3 "%s" cmd;
      run0 cmd();
      Value.checked pth
    in
    target (Key.check pth) +< Key.create pth +> check

  (** [sys cmd src tg] transforms file [src] into file [tg] using system command
      [cmd]. *)
  let sys : string -> string -> string -> (Key.t, Value.t) rule =
    fun cmd src tg ->
    let check _ =
      log_rule ~lvl:3 "sys [%s]" cmd;
      run0 cmd ();
      Value.written tg
    in
    target (Key.create tg) +< Key.create src +> check

  (** [phony cmds ?deps name] creates a rule named [name] that executes system
      commands [cmds] sequentially. Dependencies can be specified with [?deps]
      (which is by default the empty list). *)
  let phony : string list -> ?deps:(Key.t list) -> string ->
    (Key.t, Value.t) rule = fun cmds ?(deps=[]) name ->
    target (Key.fake name) |> List.fold_right depends deps |> assemble
      (fun _ -> List.iter (fun c -> run0 c ()) cmds; Value.faked ())
end
