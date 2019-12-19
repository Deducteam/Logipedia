(** Some rule making facilities. *)
open Extras
open Build

type mident = Kernel.Basic.mident
let mident_eq : mident eq = Kernel.Basic.mident_eq

(** [key_eq k l] returns if key [k] and key [l] are the equal. *)
let key_eq : [< `DkMd of mident | `SysMd of mident | `DkSig of mident] eq =
  fun k l ->
  match k, l with
  | `SysMd(k), `SysMd(l) | `DkSig(k), `DkSig(l)
  | `DkMd(k) , `DkMd(l) -> mident_eq k l
  | _                   -> false

(** [pp_key fmt k] prints key [k] to formatter [fmt]. *)
let pp_key : [> `DkMd of mident | `SysMd of mident | `DkSig of mident] pp =
  fun fmt k ->
  let module Dpp = Api.Pp.Default in
  match k with
  | `DkMd(m)  -> Format.fprintf fmt "DkMd(%a)" Dpp.print_mident m
  | `SysMd(m) -> Format.fprintf fmt "SysMd(%a)" Dpp.print_mident m
  | `DkSig(m) -> Format.fprintf fmt "DkSig(%a)" Dpp.print_mident m
  | _         -> Format.fprintf fmt "NoPrinter"

(** [mk_rule_idle k] creates a rule that does nothing. *)
let mk_rule_idle : 'k -> ('k, unit) rulem = fun key ->
  {m_creates=key; m_depends=[]; m_action = fun _ -> ()}

(** [mk_rule_sig md] creates a rule to load module [md] into the signature. *)
let mk_rule_sig : mident -> ([> `DkSig of mident], unit) rulem =
  fun md ->
  let file = Api.Dep.get_file md in
  let m_depends = Deps.deps_of_md md |> List.map (fun x -> `DkSig(x)) in
  let m_action _ =
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
    List.iter declare entries
  in
  {m_creates=`DkSig(md); m_depends; m_action}

(** [mk_rule_sys_of_dk ~entries_pp md fext outdir] allows to print
    entries in module [md] with [~entries_pp] into a file [md.fext] in
    [outdir]. *)
let mk_rule_sys_of_dk :
  entries_pp:Parsing.Entry.entry list pp -> mident -> string -> string ->
  ('k, unit) rulem = fun ~entries_pp md fext outdir ->
  let infile = Api.Dep.get_file md in
    let m_depends =
      let deps = Deps.deps_of_md md in
      `DkSig(md) :: List.map (fun x -> `DkSig(x)) deps
    in
    let m_action _ =
      let entries =
        let input = open_in infile in
        let ret = Parsing.Parser.Parse_channel.parse md input in
        close_in input;
        ret
      in
      let ochan =
        let open Filename in
        let ofile =
          (concat outdir (basename (chop_extension infile))) ^ "." ^ fext
        in
        open_out ofile
      in
      let ofmt = Format.formatter_of_out_channel ochan in
      entries_pp ofmt entries;
      close_out ochan;
    in
    {m_creates=`SysMd(md); m_depends; m_action}
