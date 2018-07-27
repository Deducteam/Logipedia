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

let mddep = Hashtbl.create 11

let iddep = Hashtbl.create 101

let update_id md id md' id' =
  let l = try Hashtbl.find iddep (md,id) with _ -> [] in
  let l' = (md',id')::l in
  Hashtbl.replace iddep (md,id) l'

let update_md md md' =
  let l = try Hashtbl.find mddep md with _ -> [] in
  let l' = md'::l in
  Hashtbl.replace mddep md l'

let new_iddep md id md' id' =
  not (Hashtbl.mem iddep (md,id)) ||
  not (List.mem (md',id') (Hashtbl.find iddep (md,id)))

let new_mddep md md' =
  md <> md' && (not (Hashtbl.mem mddep md) || not (List.mem md' (Hashtbl.find mddep md)))

let fct_insert_dependances (md,id) (md',id') =
  if new_iddep md id md' id' then
    begin
      if new_mddep md md' then
        begin
          Mongodb.insert_mdDep md md';
          update_md md md'
        end;
      Mongodb.insert_idDep md id md' id';
      update_id md id md' id'
    end

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
