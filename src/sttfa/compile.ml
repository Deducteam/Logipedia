open Ast
open Sttfadk
open Environ

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof
module PP = Api.Pp
module Denv = Api.Env.Default
module Term = Kernel.Term
module Entry = Parsing.Entry

let compile_declaration name ty =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile_type empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile__type empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    (* Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ; *)
      let te' = CTerm.compile_term empty_env a in
      Axiom (of_name name, te')
  | _ ->
    if Sttfadk.is_tyop ty then
      begin
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        TypeDecl (of_name name, arity_of_tyop ty)
      end
    else assert false

let compile_definition name ty term =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    (* Format.eprintf "[COMPILE] definition: %a@." Pp.print_name name ; *)
      Definition (of_name name, CType.compile_type empty_env a, CTerm.compile_term empty_env term)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    (* Format.eprintf "[COMPILE] theorem: %a@." Pp.print_name name ; *)
    (* The statement written and the one we get from the proof are beta,delta convertible. *)
    let j, proof = CProof.compile_proof empty_env term in
    let a' = CTerm.compile_term empty_env a in
    let proof' =
      if j.thm = a' then proof
      else
        Conv
          ( {j with thm=a'}
          , proof
          , Sttfatyping.Tracer.annotate empty_env j.thm a'
          )
    in
    Theorem (of_name name, CTerm.compile_term empty_env a, proof')
  | _ ->
    if is_tyop ty then
      begin
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        let vars,ty = CType.compile_type_definition empty_env term in
        TypeDef(of_name name, vars, ty)
      end
    else
      assert false

(* NOTE [compile_entry] assumes that the signature already contains the item.
   This is ensured by the dependency rules: any file having that need the symbol
   in the signature should depend on [`Sign(md)] (this is set in the [rules_for]
   functions in the [Makefile] modules). *)
let compile_entry md e =
  let open Parsing.Entry in
  match e with
  | Decl(_,id,_,ty)        -> compile_declaration (Basic.mk_name md id) ty
  | Def(_,id,_,Some ty,te) -> compile_definition (Basic.mk_name md id) ty te
  | Def(_)   -> failwith "Definition without types are not supported"
  | Rules(_) -> failwith "Rules are not part of the sttforall logic"
  | _        -> failwith "Dedukti Commands are not supported"
