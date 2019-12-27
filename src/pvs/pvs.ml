open Core
open Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Pvs in
  M.print_ast fmt ast

module Makefile : MAKEFILE =
struct
  open Build.Classic
  open Build_template
  open Sttfa.Makefile

  type key =
    [ `Kfile of path
    | `Ksign of mident
    | `Kchck of path
      (** A checked file. *) ]

  type value =
    [ `Vfile of float
    | `Vsign of entry list
    | `Vchck of float
      (** Checked file with time of last check. *) ]

  (** [want pth] declares path [pth] as a checked target. *)
  let want pth = `Kchck(pth)

  (** [atime p] returns the last access time of filepath [p]. *)
  let atime pth = Unix.((stat pth).st_atime)

  let valid_stored k v =
    match k, v with
    | `Kchck(p), `Vchck(t)     -> Sys.file_exists p && t >= (atime p)
    | #Basis.key, #Basis.value -> Sttfa.Makefile.Basis.valid_stored k v
    | _                        -> false

  let pp_key fmt k = match k with
    | `Kchck(p)  -> Format.fprintf fmt"%s checked" p
    | #Basis.key -> Basis.pp_key fmt k

  let key_eq k l = match k, l with
    | `Kchck(p), `Kchck(q)   -> String.equal p q
    | #Basis.key, #Basis.key -> Basis.key_eq k l
    | _                      -> false

  (** [rule_check t] specifies that file [t] should be checked with
      "proveit". *)
  let rule_check tg : (key, value) Build.Classic.rule =
    let log_rule = Build.log_rule.logger in
    let proveit _ =
      let cmd =
        Format.sprintf "proveit --importchain --scripts --force %s" tg
      in
      log_rule ~lvl:25 "%s" cmd;
      if Sys.command cmd <> 0 then log_rule ~lvl:10 "Command failure";
      `Vchck(atime tg)
    in
    target (`Kchck(tg)) +< `Kfile(tg) +> proveit

  let rules_for files mk_target =
    let entries_pp md fmt ens = Ast.compile md ens |> export fmt in
    Sttfa.Makefile.rules_for files mk_target entries_pp @
    List.map (fun f -> rule_check (mk_target f)) files
end
