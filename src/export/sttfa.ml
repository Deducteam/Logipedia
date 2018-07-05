open Ast
open Sttforall
open Environ
open Term
open Basic
open Format

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

let parameters () = Mongo.create_local_default "logipedia" "parameters"
let definitions () = Mongo.create_local_default "logipedia" "definitions"
let theoremes () = Mongo.create_local_default "logipedia" "theoremes"
let axiomes () = Mongo.create_local_default "logipedia" "axiomes"
let dependances () = Mongo.create_local_default "logipedia" "dependances"
let dependancesMod () = Mongo.create_local_default "logipedia" "dependancesMod"

let empty_doc = Bson.empty

let insert_parameter (md,id) ty =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "type" (Bson.create_string (ty)) key_doc_2 in
  let key_doc_4 = Bson.add_element "langID" (Bson.create_string ("1")) key_doc_3 in
  Mongo.insert (parameters ()) [key_doc_4]

let insert_definition (md,id) ty te =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "type" (Bson.create_string (ty)) key_doc_2 in
  let key_doc_4 = Bson.add_element "statement" (Bson.create_string (te)) key_doc_3 in
  let key_doc_5 = Bson.add_element "langID" (Bson.create_string ("1")) key_doc_4 in
  Mongo.insert (definitions ()) [key_doc_5]

let insert_theorem (md,id) te proof =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "statement" (Bson.create_string (te)) key_doc_2 in
  let key_doc_4 = Bson.add_element "proof" (Bson.create_string (proof)) key_doc_3 in
  let key_doc_5 = Bson.add_element "langID" (Bson.create_string ("1")) key_doc_4 in
  Mongo.insert (theoremes ()) [key_doc_5]

let insert_axiom (md,id) te =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "statement" (Bson.create_string (te)) key_doc_2 in
  let key_doc_4 = Bson.add_element "langID" (Bson.create_string ("1")) key_doc_3 in
  Mongo.insert (axiomes ()) [key_doc_4]

let insert_dependances (md,id) (md',id') =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
  let key_doc_3 = Bson.add_element "mdDep" (Bson.create_string (md')) key_doc_2 in
  let key_doc_4 = Bson.add_element "idDep" (Bson.create_string (id')) key_doc_3 in
  Mongo.insert (dependances ()) [key_doc_4]

let insert_dependances_module md md' =
  let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
  let key_doc_2 = Bson.add_element "mdDep" (Bson.create_string (md')) key_doc_1 in
  Mongo.insert (dependancesMod ()) [key_doc_2]

let to_string fmt = Format.asprintf "%a" fmt

let fct_insert_dependances (md,id) (md',id') =
  insert_dependances (md,id) (md',id');
  if md<>md' then insert_dependances_module md md'

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
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
      insert_parameter (of_name name) (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
      insert_parameter (of_name name) (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ;
      insert_axiom (of_name name) (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty ->
    Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ;
  | _ -> assert false

let compile_definition name ty term =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] definition: %a@." Pp.print_name name ;
      insert_definition (of_name name) (to_string Pp.print_term a) (to_string Pp.print_term term);
      matcher_dep (of_name name) a;
      matcher_dep (of_name name) term
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] theorem: %a@." Pp.print_name name ;
    insert_theorem (of_name name) (to_string Pp.print_term a) (to_string Pp.print_term term);
    matcher_dep (of_name name) a;
    matcher_dep (of_name name) term
  | _ -> assert false

let print_ast _ _ _ = failwith "test"

let print_bdd _ = failwith "test"
