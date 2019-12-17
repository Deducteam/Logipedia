open Extras
open Build

let key_eq : _ eq = fun k l ->
  match k, l with
  | `SysMd(k), `SysMd(l) -> Kernel.Basic.mident_eq k l
  | `DkMd(_), `DkMd(_)   -> true (* Only one 'dummy' rule for dks. *)
  | _                    -> false

let pp_key : _ pp = fun fmt k ->
  let module Dpp = Api.Pp.Default in
  match k with
  | `DkMd(m)  -> Format.fprintf fmt "DkMd(%a)" Dpp.print_mident m
  | `SysMd(m) -> Format.fprintf fmt "SysMd(%a)" Dpp.print_mident m

let mk_rule_idle : 'k -> ('k, _) rulem = fun key ->
  {m_creates=key; m_depends=[]; m_action = fun _ -> ()}

let mk_rule_sys_of_dk :
  entries_pp:Parsing.Entry.entry list pp ->
  keep_deps:('k list -> 'k list) ->
  Kernel.Basic.mident -> string -> string -> ('k, _) rulem =
  fun ~entries_pp ~keep_deps md fext outdir ->
  let infile = Api.Dep.get_file md in
    let m_depends =
      let input = open_in infile in
      let deps = Deps.deps_of_md input md in
      close_in input;
      keep_deps (List.map (fun x -> `DkMd(x)) deps)
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
          (concat outdir (basename (chop_extension infile))) ^ fext
        in
        open_out ofile
      in
      let ofmt = Format.formatter_of_out_channel ochan in
      entries_pp ofmt entries;
      close_out ochan;
    in
    {m_creates=`SysMd(md); m_depends; m_action}
