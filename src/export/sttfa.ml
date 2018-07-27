open Ast
open Sttforall
open Environ
open Term
open Basic
open Format

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

let sys = "sttfa"

let to_string fmt = Format.asprintf "%a" fmt

let fct_insert_dependances (md,id) (md',id') =
  Mongodb.insert_idDep md id md' id';
  if md<>md' then Mongodb.insert_mdDep md md'

let rec matcher_dep (md,id) a =
  match a with
  |Kind -> ()
  |Type(_) -> ()
  |DB(_, _, _) -> ()
  |Const(_, name') -> fct_insert_dependances (md,id) (of_name name')
  |App(ter, term, term_lst) -> matcher_dep (md,id) ter; matcher_dep (md,id) term; List.iter (matcher_dep (md,id)) term_lst
  |Lam(_, _, _, term) -> matcher_dep (md,id) term
  |Pi(_, _, ter, term) -> matcher_dep (md,id) ter; matcher_dep (md,id) term

let compile_declaration name ty =
  let md,id = of_name name in
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
      Mongodb.insert_constant sys "" md id (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
      Mongodb.insert_constant sys "" md id (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ;
      Mongodb.insert_axiom sys "" md id (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty -> (* Partial function *)
    Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ;
    Mongodb.insert_constant sys "" md id (to_string Pp.print_term ty);
  | _ -> assert false

let compile_definition name ty term =
  let md,id = of_name name in
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] definition: %a@." Pp.print_name name ;
      Mongodb.insert_definition sys "def" md id (to_string Pp.print_term a) (to_string Pp.print_term term);
      matcher_dep (of_name name) a;
      matcher_dep (of_name name) term
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] theorem: %a@." Pp.print_name name ;
    Mongodb.insert_theorem sys "def" md id (to_string Pp.print_term a) (to_string Pp.print_term term);
    matcher_dep (of_name name) a;
    matcher_dep (of_name name) term
  | _ -> assert false

let print_ast _ _ _ = ()
let print_bdd _ = ()
