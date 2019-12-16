(** Signature of a system. *)

(** Ast and interactions with Dk files that a system must provide. *)
module type AST =
sig
  type t

  val compile : Kernel.Basic.mident -> Parsing.Entry.entry list -> t
  (** [compile md es] builds an ast out of a list of Dedukti entries
      [es] coming from module [md]. *)

  val decompile : t -> Parsing.Entry.entry list
  (** [decompile ast] returns the list of Dedukti entries coming from
      ast [ast]. *)
end

(** Type of a system. *)
module type S =
sig
  module Ast : AST

  module Mid : Middleware.S
  (** Middleware used for the json export. *)

  val export : Ast.t -> Format.formatter -> unit
  (** [export ast fmt] exports abstract syntax tree [ast] to formatter
      [fmt] in the syntax of the system. *)
end

module Production =
struct
  open Core.Extras
  open Core.Build

  type key =
    | SysMd of Kernel.Basic.mident
    | DkMd of Kernel.Basic.mident

  let pp_key : key pp = fun fmt k ->
    let module Dpp = Api.Pp.Default in
    match k with
    | DkMd(m)  -> Format.fprintf fmt "DkMd(%a)" Dpp.print_mident m
    | SysMd(m) -> Format.fprintf fmt "SysMd(%a)" Dpp.print_mident m

  let key_eq : key eq = fun k l ->
    match k, l with
    | SysMd(k), SysMd(l) -> Kernel.Basic.mident_eq k l
    | DkMd(_), DkMd(_) -> true (* Only one 'dummy' rule for dks. *)
    | _                -> false

  let rulem_dk_idle : (key, unit) rulem =
    {m_creates=DkMd(Kernel.Basic.mk_mident "");
     m_depends=[]; m_action = fun _ -> ()}

  (** [rulem_of_file Sys infile fext outdir] outputs the rule to create the
      system [Sys] counterpart of file [infile]. The output file will be in
      directory [outdir] and with extension [fext]. *)
  let rulem_of_file : (module S) -> string -> string -> string ->
    (key, unit) rulem = fun (module Syst) infile fext outdir ->
    let md = Api.Env.Default.init infile in
    let m_depends =
      let input = open_in infile in
      let deps = Core.Deps.deps_of_md input md in
      close_in input;
      DkMd(md) :: List.map (fun x -> DkMd(x)) deps
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
      Format.printf "Here we are@.";
      let ast = Syst.Ast.compile md entries in
      Format.printf "there yet?@.";
      Format.formatter_of_out_channel ochan |>
      Syst.export ast;
      close_out ochan;
    in
    {m_creates=SysMd(md); m_depends; m_action}
end
