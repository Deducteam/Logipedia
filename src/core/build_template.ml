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
  let open Filename in
  chop_extension file <.> "dko"

let dk_of : string -> string = fun fp -> let open Filename in
  (dirname fp) </> !/fp <.> "dk"

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
    | Phon of string
    (** A phony rule with a name. *)

  (** [eq k l] is equality among keys. *)
  let eq k l = match k, l with
    | File(p), File(q)
    | Phon(p), Phon(q) -> String.equal p q
    | _                -> false

  (** [pp fmt k] prints key [k] to formatter [fmt]. *)
  let pp : t pp = fun fmt k ->
    let out ofmt = Format.fprintf fmt ofmt in
    match k with
    | File(p) -> out "File(%s)" p
    | Phon(n) -> out "Phon(%s)" n

  (** [exists p] requires the existence of file [p]. *)
  let exists : string -> t = fun p -> File(p)

  (** [create f] requires the creation of file [f]. *)
  let create : string -> t = fun p -> File(p)

  (** [fake n] requires the phony rule of name [n]. *)
  let fake : string -> t = fun name -> Phon(name)
end

(** Definitions of values and helper functions. *)
module Value = struct
  (** Values that can be requested from a build run. *)
  type t =
    | Wfil of float
    (** The modification time of a written file. The 'intuitive'
        value, the content of the file, is already saved on the
        filesystem, so we don't keep it. *)
    | Rfil of float
    (** The access time of a read file. *)
    | Phon of unit
    (** Result of a phony rule. *)

  (** [checked pth] sets filepath [pth] as checked. *)
  let written : string -> t = fun pth -> Wfil(mtime pth)

  (** [checked pth] sets filepath [pth] as checked. *)
  let checked : string -> t = fun pth -> Rfil(atime pth)

  (** [faked ()] returns the value for a phony target. *)
  let faked : unit -> t = fun () -> Phon(())
end

(** [valid_store key value] tells whether value [value] computed from key [key]
    is up to date. *)
let valid_stored : Key.t -> Value.t -> bool = fun k v ->
  let module K = Key in let module V = Value in
  match k, v with
  | K.File(p), V.Wfil(t) -> Sys.file_exists p && t >= mtime p
  | K.File(p), V.Rfil(t) -> Sys.file_exists p && t >= atime p
  | K.Phon(_), V.Phon(_) -> false
  | _                    -> invalid_arg "valid_stored"

(** Generation of rules. *)
module Rule =
struct
  (** [need pth] creates a rule that verifies if file [pth] exists. *)
  let need : string -> (Key.t, Value.t) rule = fun pth ->
    let exists _ =
      if not (Sys.file_exists pth) then exit_with "missing [%s]" pth else
      Value.checked pth
    in
    target (Key.create pth) +> exists

  (** [dko f] creates a rule to create the Dedukti object file [f]. *)
  let dko : string -> (Key.t, Value.t) rule = fun out ->
    let dir = Filename.dirname out in
    let src = dk_of out in
    let o_deps =
      Deps.deps_of_md (init src) |> DkTools.MdSet.to_seq |>
      Seq.map (fun m -> get_file m |> objectify) |>
      Seq.map Key.create |> List.of_seq
    in
    let dkcheck _ =
      let incl = get_path () in
      let includes = List.map ((^) "-I ") incl |> String.concat " " in
      let cmd =
        Format.sprintf "dkcheck -e %s -I %s %s 2> /dev/null"
          includes dir src in
      log_rule ~lvl:3 "%s" cmd;
      run0 cmd ();
      (* NOTE [init] actually loads [src] into signature *)
      ignore @@ Api.Env.Default.init src;
      Value.written out
    in
    target (Key.create out) +< Key.create src |>
    List.fold_right depends o_deps |>
    assemble dkcheck

  (** [entry_printer target entries_pp md] creates a rule that prints entries of
      module [md] with [entries_pp md] into file [target]. The target depends on
      the signature of the module. *)
  let entry_printer : string -> (Mident.t -> entry list pp) -> Mident.t ->
    (Key.t, Value.t) rule = fun tg pp_entries md ->
    let pp_entries = pp_entries md in
    let deps = Deps.deps_of_md md in
    let src = DkTools.get_file md in
    let print _ =
      log_rule ~lvl:3 "printing [%s]" tg;
      let ochan = open_out tg in
      let ofmt = Format.formatter_of_out_channel ochan in
      let entries =
        let inchan = open_in src in
        let e = Parsing.Parser.Parse_channel.parse md inchan in
        close_in inchan;
        e
      in
      pp_entries ofmt entries;
      close_out ochan;
      Value.written tg
    in
    let deps =
      let mds = DkTools.MdSet.to_seq deps in
      let srco = Seq.map DkTools.get_file mds |> Seq.map objectify in
      Seq.map Key.create srco |> List.of_seq
    in
    let obj = Key.create (src |> objectify) in
    target (Key.create tg) +< obj |> List.fold_right depends deps |>
    assemble print

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
