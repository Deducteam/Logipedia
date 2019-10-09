open Ast
open Sttfadk
open Environ


module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

module B = Kernel.Basic
module T = Kernel.Term
module Env = Api.Env
module Ent = Parsers.Entry

let compile_declaration dkenv name ty =
  match ty with
  | T.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile_type dkenv empty_env a in
      Parameter (of_name name, ty')
  | T.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile__type dkenv empty_env a in
      Parameter (of_name name, Ty ty')
  | T.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    (* Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ; *)
      let te' = CTerm.compile_term dkenv empty_env a in
      Axiom (of_name name, te')
  | _ ->
    if is_tyop ty then
      begin
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        TypeDecl (of_name name, arity_of_tyop ty)
      end
    else
      assert false

let compile_definition dkenv name ty (term:T.term) =
  match ty with
  | T.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    (* Format.eprintf "[COMPILE] definition: %a@." Pp.print_name name ; *)
    Definition (of_name name, CType.compile_type dkenv empty_env a,
                CTerm.compile_term dkenv empty_env term)
  | T.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    (* Format.eprintf "[COMPILE] theorem: %a@." Pp.print_name name ; *)
    (* The statement written and the one we get from the proof are beta,delta convertible. *)
    let j, proof = CProof.compile_proof dkenv empty_env term in
    let a' = CTerm.compile_term dkenv empty_env a in
    let proof' =
      let (module Tracer) = Sttfatyping.mk_tracer dkenv in
      if j.thm = a' then proof
      else
        Conv
          ( {j with thm=a'}
          , proof
          , Tracer.annotate empty_env j.thm a'
          )
    in
    Theorem (of_name name, CTerm.compile_term dkenv empty_env a, proof')
  | _ ->
    if is_tyop ty then
      begin
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        let vars,ty = CType.compile_type_definition dkenv empty_env term in
        TypeDef(of_name name, vars, ty)
      end
    else
      assert false

let compile_entry dkenv e =
  let md = Env.get_name dkenv in
    match e with
    | Ent.Decl (lc, id, st, ty) ->
      ( Env.declare dkenv lc id st ty;
        compile_declaration dkenv (B.mk_name md id) ty )
    | Ent.Def (lc, id, opaque, Some ty, te) ->
      ( Env.define dkenv lc id opaque te (Some ty);
        compile_definition dkenv (B.mk_name md id) ty te  )
    | Ent.Def _ -> failwith "Definition without types are not supported"
    | Ent.Rules _ -> failwith "Rules are not part of the sttforall logic"
    | _ -> failwith "Dedukti Commands are not supported"

module ItemsBuilder =
struct
  type t = Ast.item list

  let _acc = ref []

  let handle_entry : Env.t -> Ent.entry -> unit = fun env ent ->
    _acc := compile_entry env ent :: !_acc

  let get_data () = !_acc
end


module AstBuilder = struct
  type t = Ast.ast

  let _ast = ref { Ast.md = "" ; dep = Ast.QSet.empty ; items = [] }

  let handle_entry : Env.t -> Ent.entry -> unit = fun _ _ -> ()

  let get_data () = !_ast
end

module DepBuilder =
struct
  type t = Ast.QSet.t

  let _acc = ref Ast.QSet.empty

  let handle_entry : Env.t -> Ent.entry -> unit = fun env ent ->
    let md = Env.get_name env in
    let d = Deps.dep_of_entry [Sttfadk.sttfa_module;md] ent in
    _acc := Ast.QSet.union !_acc d

  let get_data () = !_acc
end
