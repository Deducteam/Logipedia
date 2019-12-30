(** Some rule making facilities. *)
open Extras
open Console
open Build.Classic

(** Directory where files are exported. *)
let outdir : string option ref = ref None

(** All Dedukti functions needed. *)
module DkTools =
struct
  type mident = Kernel.Basic.mident
  let mident_eq : mident eq = Kernel.Basic.mident_eq
  let pp_mident : mident pp = Api.Pp.Default.print_mident

  let get_file : mident -> string = Api.Dep.get_file
  let init : string -> mident = Api.Env.Default.init
  type entry = Parsing.Entry.entry

  let get_path : unit -> string list = Kernel.Basic.get_path
end
open DkTools

(** A logger to be used in rules. *)
let log_rule = Build.log_rule.logger

(** [objectify pth] transforms [f.dk] in [f.dko]. If [file] is not of the form
    [f.dk],
    @raise Invalid_argument *)
let objectify : string -> string = fun file ->
  if Filename.extension file <> ".dk" then invalid_arg "objectify";
  file ^ "o"

(** [mtime p] returns the modification time of a file. *)
let mtime : string -> float = fun string -> Unix.((stat string).st_mtime)

(** [atime p] returns the modification time of a file. *)
let atime : string -> float = fun string -> Unix.((stat string).st_atime)

(** [run cmd] creates an action that runs command [cmd]. The command should
    return 0 in case of success. *)
let run : string -> _ -> unit = fun cmd _ ->
  if Sys.command cmd <> 0 then log_rule ~lvl:10 "command failed [%s]" cmd

(** Definition of keys used and helper functions. *)
module Key =
struct
  (** Keys used. *)
  type t =
    [ `K_file of string
    (** A file to create. *)
    | `K_sign of mident
    (** The signature of a module. *)
    | `K_cfil of string
    (** A file to check. *)
    | `K_phon of string
      (** A phony rule with a name. *) ]

  (** [eq k l] is equality among keys. *)
  let eq k l = match k, l with
    | `K_file(p), `K_file(q)
    | `K_phon(p), `K_phon(q)
    | `K_cfil(p), `K_cfil(q) -> String.equal p q
    | `K_sign(m), `K_sign(n) -> mident_eq m n
    | _                      -> false

  (** [pp fmt k] prints key [k] to formatter [fmt]. *)
  let pp : t pp = fun fmt k ->
    let out ofmt = Format.fprintf fmt ofmt in
    match k with
    | `K_file(p) -> out "File(%s)" p
    | `K_cfil(p) -> out "Chck(%s)" p
    | `K_sign(m) -> out "Load(%a)" pp_mident m
    | `K_phon(n) -> out "Phon(%s)" n

  (** [check f] asks to check file [f]. *)
  let check : string -> [> `K_cfil of _] = fun p -> `K_cfil(p)

  (** [exists p] requires the existence of file [p]. *)
  let exists = check

  (** [create f] requires the creation of file [f]. *)
  let create : string -> [> `K_file of _] = fun p -> `K_file(p)

  (** [load m] requires to load module [m] into the signature. *)
  let load : mident -> [> `K_sign of mident] = fun md -> `K_sign(md)

  (** [fake n] requires the phony rule of name [n]. *)
  let fake : string -> [> `K_phon of string] = fun name -> `K_phon(name)
end

(** Definitions of values and helper functions. *)
module Value =
struct
  (** Values that can be requested from a build run. *)
  type t =
    [ `V_wfil of float
    (** The modification time of a written file. The 'intuitive' value, the
        content of the file, is already saved on the filesystem, so we don't keep
        it. *)
    | `V_rfil of float
    (** The access time of a read file. *)
    | `V_sign of entry list
    (** The content of a signature, that is, the entries. *)
    | `V_phon of unit
      (** Result of a phony rule. *) ]

  (** [checked pth] sets filepath [pth] as checked. *)
  let written : string -> [> `V_wfil of float] = fun pth -> `V_wfil(mtime pth)

  (** [loaded e] returns the value containing entries [e]. *)
  let loaded : entry list -> [> `V_sign of _] = fun ens -> `V_sign(ens)

  (** [checked pth] sets filepath [pth] as checked. *)
  let checked : string -> [> `V_rfil of float] = fun pth -> `V_rfil(atime pth)

  (** [faked ()] returns the value for a phony target. *)
  let faked : unit -> [> `V_phon of unit] = fun () -> `V_phon(())

  let is_vsign : [> `V_sign of _] -> bool = function
    | `V_sign(_) -> true
    | _          -> false

  let to_entries : [> `V_sign of _] -> entry list = function
    | `V_sign(x) -> x
    | _          -> invalid_arg "to_entries"

end

(** [valid_store key value] tells whether value [value] computed from key [key]
    is up to date. *)
let valid_stored : Key.t -> Value.t -> bool = fun k v -> match k, v with
  | `K_file(p), `V_wfil(t) -> Sys.file_exists p && t >= mtime p
  | `K_file(p), `V_rfil(t) -> Sys.file_exists p && t >= atime p
  | `K_cfil(p), `V_rfil(t) -> Sys.file_exists p && t >= atime p
  | `K_sign(_), `V_sign(_)
  | `K_phon(_), `V_phon(_) -> false
  | _                      -> invalid_arg "valid_stored"

(** Generation of rules. *)
module Rule =
struct
  (** [need pth] creates a rule that verifies if file [pth] exists. *)
  let need : string -> (_, _) rule = fun pth ->
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
      let cmd = Format.sprintf "dkcheck -e %s -I %s %s" includes dir file in
      log_rule ~lvl:25 "%s" cmd;
      run cmd ();
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
      log_rule ~lvl:25 "printing [%s]" tg;
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

  (** [check cmd pth] creates a rule to check file [pth] with command [cmd] which
      should return 0 in case of success. *)
  let check : string -> string -> (Key.t, Value.t) rule = fun cmd pth ->
    let check _ =
      log_rule ~lvl:25 "%s" cmd;
      run cmd();
      Value.checked pth
    in
    target (Key.check pth) +< Key.create pth +> check

  (** [sys cmd src tg] transforms file [src] into file [tg] using system command
      [cmd]. *)
  let sys : string -> string -> string -> (Key.t, Value.t) rule = fun cmd src tg ->
    let check _ =
      log_rule ~lvl:25 "sys [%s]" cmd;
      run cmd ();
      Value.written tg
    in
    target (Key.create tg) +< Key.create src +> check

  (** [phony cmds ?deps name] creates a rule named [name] that executes system
      commands [cmds] sequentially. Dependencies can be specified with [?deps]
      (which is by default the empty list). *)
  let phony : string list -> ?deps:(Key.t list) -> string ->
    (Key.t, Value.t) rule = fun cmds ?(deps=[]) name ->
    target (Key.fake name) |> List.fold_right depends deps |> assemble
      (fun _ -> List.iter (fun c -> run c ()) cmds; Value.faked ())
end
