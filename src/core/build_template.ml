(** Some rule making facilities. *)
open Extras
open Console
open Build

(** Some shorthands. *)
type mident = Kernel.Basic.mident
let mident_eq : mident eq = Kernel.Basic.mident_eq
type entry = Parsing.Entry.entry

(** Type of path. *)
type path = string

(** Keys are files or signatures. *)
type key =
  | Kfile of path
  (** A filepath. *)
  | Ksign of mident
  (** The signature of a module. *)

(** [time p] returns the modification time of a file. *)
let time : path -> float = fun path -> Unix.((stat path).st_mtime)

(** Values are either files or signatures. *)
type value =
  | Vfile of path * float
  (** A filepath and its last modification time. *)
  | Vsign of entry list
  (** The entries of a signature. *)

(** [key_eq k l] returns true if key [k] and key [l] are the equal. *)
let key_eq : key eq = fun k l ->
  match k, l with
  | Kfile(p), Kfile(q) -> String.equal p q
  | Ksign(m), Ksign(n) -> mident_eq m n
  | _                  -> false

let pp_key : key pp = fun fmt k ->
  let module Dpp = Api.Pp.Default in
  let out p = Format.fprintf fmt p in
  match k with
  | Kfile(p) -> out "File(%s)" p
  | Ksign(m) -> out "Sign(%a)" Dpp.print_mident m

(** [valid_stored k v] returns true *)
let valid_stored : key -> value -> bool = fun k v ->
  match k, v with
  | Kfile(p), Vfile(_,t) -> Sys.file_exists p && t >= (time p)
  | Ksign(_), Vsign(_)   -> false
  (* Rebuild avoided by dkos for the moment *)
  | _                    -> false

(** [mk_rule_sig md] creates a rule to load module [md] into the signature. *)
let mk_sigrule : mident -> (key, value) rulem = fun md ->
  let file = Api.Dep.get_file md in
  let m_creates = Ksign(md) in
  let m_depends = Deps.deps_of_md md |> List.map (fun x -> Ksign(x)) in
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
    Vsign(entries)
  in
  {m_creates; m_depends; m_action}

(** [mk_sysrule ~target ~entries_pp md] creates a rule that prints entries of
    module [md] with [~entries_pp] into file [~target]. *)
let mk_sysrule : target:string -> entries_pp:entry list pp -> mident ->
  (key, value) rulem = fun ~target ~entries_pp md ->
  let m_creates = Kfile(target) in
  let m_depends = [Ksign(md)] in
  let m_action entries =
    if !log_enabled then log "[build] [%a]" pp_key m_creates;
    let ochan = open_out target in
    let ofmt = Format.formatter_of_out_channel ochan in
    match entries with
    | [Vsign(entries)] ->
      entries_pp ofmt entries;
      close_out ochan;
      Vfile(target, time target)
    | _                -> assert false
  in
  {m_creates; m_depends; m_action}

(** [want pth] creates the key corresponding to filepath [path]. *)
let want : path -> key = fun pth -> Kfile(pth)
