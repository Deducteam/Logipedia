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

(** Module taking in charge export of modules. *)
module type S =
sig
  type key
  type value

  val key_eq : key eq
  val pp_key : key pp
  val valid_stored : key -> value -> bool
  val want : path -> key
  val rules_for : (path * mident) list -> (key, value) rule list
end

(** Provides some standard functions for Dedukti related operations and file
    manipulatons. *)
module Dk =
struct
  (** Keys are either files or signatures. *)
  type key =
    [ `Kfile of path
    (** A filepath. *)
    | `Ksign of mident
      (** The signature of a module. *) ]

  (** Values are either the created files or the loaded signatures. *)
  type value =
    [ `Vfile of path * float
    (** A filepath along its last modification time (used to verify validity of
        a file from the database). *)
    | `Vsign of entry list
      (** Signature and the entries that are in it. *) ]

  (** [pp_key fmt key] prints key [key] to formatter [fmt]. *)
  let pp_key : key pp = fun fmt k ->
    let out f = Format.fprintf fmt f in
    match k with
    | `Kfile(pth) -> out "`Kfile(%s)" pth
    | `Ksign(md)  -> out "`Ksign(%a)" Api.Pp.Default.print_mident md

  (** [key_eq k l] returns whether keys [k] and [l] are equal. *)
  let key_eq : key eq = fun k l ->
    match k, l with
    | `Kfile(p), `Kfile(q) -> String.equal p q
    | `Ksign(m), `Ksign(n) -> mident_eq m n
    | _                    -> false

  (** [want pth] creates the key corresponding to filepath [path]. *)
  let want : path -> key = fun pth -> `Kfile(pth)

  (** [time p] returns the modification time of a file. *)
  let time : path -> float = fun path -> Unix.((stat path).st_mtime)

  (** [valid_stored k v] returns true *)
  let valid_stored : key -> value -> bool = fun k v ->
    match k, v with
    | `Kfile(p), `Vfile(_,t) -> Sys.file_exists p && t >= (time p)
    | `Ksign(_), `Vsign(_)   -> false
    (* Rebuild avoided by dkos for the moment *)
    | _                      -> false

  (** [is_vsign v] returns whether value [v] is a signature. *)
  let is_vsign : [> `Vsign of entry list] -> bool = function
    | `Vsign(_) -> true
    | _         -> false

  (** [to_entries v] returns the list of entries associated to value [v] or
      @raise Invalid_argument *)
  let to_entries : [> `Vsign of entry list] -> entry list = function
    | `Vsign(x) -> x
    | _         -> invalid_arg "Dk.to_entries"

  (** [mk_sigrule md] creates a rule to load module [md] into the signature and
      compute the entries of [md]. *)
  let mk_sigrule : mident -> (key, value) rule = fun md ->
    let file = Api.Dep.get_file md in
    let m_creates = `Ksign(md) in
    let m_depends = Deps.deps_of_md md |> List.map (fun x -> `Ksign(x)) in
    let m_action _ =
      if !log_enabled then log "[build] target [%a]" pp_key m_creates;
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
    {m_creates; m_depends; m_action}
end
