open Ast
open Sttforall
open Environ

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

let compile_declaration name ty =
  Format.eprintf "Compile %a@." Pp.print_name name ;
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      let ty' = CType.compile_type empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      let ty' = CType.compile__type empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let te' = CTerm.compile_term empty_env a in
      Axiom (of_name name, te')
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty ->
      TyOpDef (of_name name, 0)
  | _ -> assert false


let compile_definition name ty term =
  Format.eprintf "Compile %a@." Pp.print_name name ;
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      Definition
        (of_name name, CType.compile_type empty_env a, CTerm.compile_term empty_env term)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let j, proof = CProof.compile_proof empty_env term in
      let a' = CTerm.compile_term empty_env a in
      let proof' =
        if j.thm = a' then proof
        else
          Conv
            ( {j with thm=a'}
            , proof
            , CProof.Tracer.annotate empty_env (Decompile.decompile_term [] j.thm) a
            )
      in
      Theorem (of_name name, CTerm.compile_term empty_env a, proof')
  | _ -> assert false
