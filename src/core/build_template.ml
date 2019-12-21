(** Some rule making facilities. *)
open Extras
open Console
open Build

type mident = Kernel.Basic.mident
let mident_eq : mident eq = Kernel.Basic.mident_eq

type path = string

type key =
  [ `Kfile of path
  | `Ksign of mident ]

type value = [ `Vfile of Digest.t | `Vsign ]

(** [key_eq k l] returns true if key [k] and key [l] are the equal. *)
let key_eq : key eq = fun k l ->
  match k, l with
  | `Kfile(p), `Kfile(q) -> String.equal p q
  | `Ksign(m), `Ksign(n) -> mident_eq m n
  | _                    -> false

let pp_key : key pp = fun fmt k ->
  let module Dpp = Api.Pp.Default in
  let out p = Format.fprintf fmt p in
  match k with
  | `Kfile(p) -> out "File(%s)" p
  | `Ksign(m) -> out "Sign(%a)" Dpp.print_mident m

(** [mk_rule_sig md] creates a rule to load module [md] into the signature. *)
let mk_rule_sig : mident -> ([> `Ksign of mident], [> `Vsign]) rulem = fun md ->
  let file = Api.Dep.get_file md in
  let m_creates = `Ksign(md) in
  let m_depends = Deps.deps_of_md md |> List.map (fun x -> `Ksign(x)) in
  let m_action _ =
    if !log_enabled then log "[build] [%a]" pp_key m_creates;
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
    `Vsign
  in
  {m_creates; m_depends; m_action}

(** [mk_rule_sys_of_dk ~entries_pp md fext outdir] allows to print
    entries in module [md] with [~entries_pp] into a file [md.fext] in
    [outdir]. *)
let mk_rule_sys_of_dk :
  entries_pp:Parsing.Entry.entry list pp -> mident -> string -> string ->
  ([> `Kfile of path], [> `Vfile of Digest.t]) rulem =
  fun ~entries_pp md fext outdir ->
  let infile = Api.Dep.get_file md in
  let m_creates = `Kfile(infile) in
  let m_depends =
    let deps = Deps.deps_of_md md in
    `Ksign(md) :: List.map (fun x -> `Ksign(x)) deps
  in
  let m_action _ =
    if !log_enabled then log "[build] [%a]" pp_key m_creates;
    let entries =
      let input = open_in infile in
      let ret = Parsing.Parser.Parse_channel.parse md input in
      close_in input;
      ret
    in
    let ofile =
      let open Filename in
      (concat outdir (basename (chop_extension infile))) ^ "." ^ fext
    in
    let ochan = open_out ofile in
    let ofmt = Format.formatter_of_out_channel ochan in
    entries_pp ofmt entries;
    close_out ochan;
    `Vfile(Digest.file ofile)
  in
  {m_creates; m_depends; m_action}
