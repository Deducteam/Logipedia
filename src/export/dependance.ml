open Ast
open Sttforall
open Environ

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

let dependances = Mongo.create_local_default "logipedia" "dependances"


let empty_doc = Bson.empty;;

let insert_dependances (md,id) elem = let key_doc_1 = Bson.add_element "md" (Bson.create_string (md)) empty_doc in
                                        let key_doc_2 = Bson.add_element "nameID" (Bson.create_string (id)) key_doc_1 in
                                          let key_doc_3 = Bson.add_element "elem" (Bson.create_string (elem)) key_doc_2
                                            Mongo.insert parameters [key_doc_4];;

                                            
let to_string fmt = Format.asprintf "%a" fmt

let compile_declaration name ty =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
      insert_parameter (of_name name) (to_string Pp.print_term a)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
      insert_parameter (of_name name) (to_string Pp.print_term a)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ;
      insert_axiom (of_name name) (to_string Pp.print_term a)
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty ->
    Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ;
  | _ -> assert false

let compile_definition name ty term =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] definition: %a@." Pp.print_name name ;
      insert_definition (of_name name) (to_string Pp.print_term a) (to_string Pp.print_term term)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] theorem: %a@." Pp.print_name name ;
    insert_theorem (of_name name) (to_string Pp.print_term a) (to_string Pp.print_term term)
  | _ -> assert false
