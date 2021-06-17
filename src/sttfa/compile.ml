open Core
open Console
open Extras
open Ast
open Sttfadk
open Environ

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof
module Term = Kernel.Term
module Entry = Parsers.Entry

let log_sttfa = new_logger "stfc"
let log_sttfa = log_sttfa.logger

let compile_declaration denv name ty =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile_type denv empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile__type denv empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    (* Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ; *)
      let te' = CTerm.compile_term denv empty_env a in
      Axiom (of_name name, te')
  | _ ->
    if Sttfadk.is_tyop ty then
      begin
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        TypeDecl (of_name name, arity_of_tyop ty)
      end
    else assert false

let compile_definition denv name ty term =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Definition(of_name name, CType.compile_type denv empty_env a, CTerm.compile_term denv empty_env term)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    (* The statement written and the one we get from the proof are beta,delta convertible. *)
    let j, proof = CProof.compile_proof denv empty_env term in
    let a' = CTerm.compile_term denv empty_env a in
    let proof' =
      if j.thm = a' then proof
      else
        Conv
          ( {j with thm=a'}
          , proof
          , Sttfatyping.Tracer.annotate denv empty_env j.thm a'
          )
    in
    Theorem (of_name name, CTerm.compile_term denv empty_env a, proof')
  | _ ->
    if is_tyop ty then
      begin
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        let vars,ty = CType.compile_type_definition denv empty_env term in
        TypeDef(of_name name, vars, ty)
      end
    else
      assert false

let compile_entry denv : Basic.mident * Entry.entry -> item =
  fun (md, e) ->
  let open Parsers.Entry in
  let module Ev = Api.Env in
  let module Pp = Api.Pp.Default in
  match e with
  | Decl(l,id,sc,st,ty) ->
    Ev.declare denv l id sc st ty;
    log_sttfa ~lvl:6 "compiling decl [%a]" Pp.print_ident id;
    compile_declaration denv (Basic.mk_name md id) ty
  | Def(l,id,sc,f,Some ty,te) ->
    Ev.define denv l id sc f te (Some ty);
    log_sttfa ~lvl:6 "compiling def [%a]" Pp.print_ident id;
    compile_definition denv (Basic.mk_name md id) ty te
  | Def(_)   -> exit_with "Definition without types are not supported"
  | Rules(_) -> exit_with "Rules are not part of the sttforall logic"
  | _        -> exit_with "Dedukti Commands are not supported"

(** Memoized [compile_entry]. *)
let compile_entry denv : Basic.mident -> Entry.entry -> item =
  let eq (m, e) (m', e') = Kernel.Basic.mident_eq m m' && Stdlib.(=) e e' in
  let f = memoize ~eq (compile_entry denv) in fun m e -> f (m, e)
