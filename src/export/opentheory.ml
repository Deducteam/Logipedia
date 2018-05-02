open Ast
open Compile

type proof_ctx = (string * _te) list

type env = Compile.env

let add_ty_var env var =
  let open Basic in
  let open Sttforall in
  { env with
    k= env.k + 1
  ; ty= var :: env.ty
  ; dk=
      (dloc, mk_ident var, Term.mk_Const dloc (mk_name sttfa_module sttfa_type))
      :: env.dk }

let add_te_var env var ty' =
  let open Basic in
  let ty = Decompile.decompile__type 0  env.ty ty' in
  { env with
    k = env.k + 1;
    te= (var, ty') :: env.te; dk= (dloc, mk_ident var, ty) :: env.dk
  }

let rec print_list pp oc = function
  | [] -> Printf.fprintf oc "Nil\n"
  | x::t ->
    print_list pp oc t;
    pp oc x;
    Printf.fprintf oc "Cons\n"

let cur_md = ref ""

let sanitize id = id

let print_var oc id = Printf.fprintf oc "\"%s\"\n" (sanitize id)

let print_name oc (md,id) =
  let id = sanitize id in
  if !cur_md = md then
    Printf.fprintf oc "\"%s\"\n" id
  else
    Printf.fprintf oc "\"%s.%s\"\n" md id

let print_tyop oc tyop =
  print_name oc tyop;
  Printf.fprintf oc "typeOp\n"

let print_ty_var oc var = print_var oc var

let rec print__ty oc = function
  | TyVar(var) ->
    print_ty_var oc var;
    Printf.fprintf oc "varType\n"
  | Arrow(_tyl,_tyr) ->
    print_list print__ty oc [_tyl;_tyr];
    print_tyop oc (!cur_md,"->"); (* small hack *)
    Printf.fprintf oc "opType\n"
  | TyOp(tyop, _tys) ->
    print_list print__ty oc _tys;
    print_tyop oc tyop;
    Printf.fprintf oc "opType\n"
  | Prop ->
    print_tyop oc (!cur_md, "bool") (* small hack *)

let rec print_ty oc = function
  | ForallK(var, ty) ->
    print_ty oc ty
  | Ty(_ty) -> print__ty oc _ty

let print_te_var oc var ty =
  print__ty oc ty;
  print_var oc var

let print_const oc cst =
  print_name oc cst;
  Printf.fprintf oc "const\n"

let print_forall_const oc =
  print_var oc "!";
  Printf.fprintf oc "const\n"

let print_impl_const oc =
  print_var oc "==>";
  Printf.fprintf oc "const\n"

let print_forall_ty oc _ty =
  let fty = Arrow(Arrow(_ty, Prop), Prop) in
  print__ty oc fty

let print_impl_ty oc =
  let ity = Arrow(Prop, Arrow(Prop, Prop)) in
  print__ty oc ity

let rec print__te ctx oc = function
  | TeVar(var) ->
    let _ty = List.assoc var ctx.te in
    print_te_var oc var _ty
  | Abs(var,_ty,_te) ->
    let ctx' = add_te_var ctx var _ty in
    print__te ctx' oc _te;
    print_te_var oc var _ty;
    Printf.fprintf oc "absTerm\n"
  | App(_tel,_ter) ->
    print__te ctx oc _ter;
    print__te ctx oc _tel;
    Printf.fprintf oc "app\n"
  | Forall(var,_ty,_te) ->
    print_forall_term ctx oc var _ty _te
  | Impl(_tel,_ter) ->
    print_impl_term ctx oc _tel _ter
  | AbsTy(var, _te) ->
    let ctx' = add_ty_var ctx var in
    print__te ctx' oc _te
  | Cst(cst, _tys) ->
    let open Basic in
    let name = mk_name (mk_mident (fst cst)) (mk_ident (snd cst)) in
    let _tys' = List.map (Decompile.decompile__type 0 ctx.ty) _tys in
    let cst' =
      match _tys' with
      | [] -> Term.mk_Const dloc name
      | x::t -> Term.mk_App (Term.mk_Const dloc name) x t
    in
    Pp.print_db_enabled := true;
    Format.eprintf "%a@." Pp.print_term cst';
    match Env.infer ~ctx:ctx.dk cst' with
    | OK _ty ->
      let _ty' = Compile.compile__type ctx _ty in
      print__ty oc _ty';
      print_const oc cst;
      Printf.fprintf oc "constTerm\n"
    | Err err -> Errors.fail_env_error err

and print_forall_term ctx oc var _ty _te =
  print__te ctx oc (Abs(var,_ty,_te));
  print_forall_ty oc _ty;
  print_forall_const oc;
  Printf.fprintf oc "constTerm\n";
  Printf.fprintf oc "appTerm\n"

and print_impl_term ctx oc _tel _ter =
  print__te ctx oc _ter;
  print__te ctx oc _tel;
  print_impl_ty oc;
  print_impl_const oc;
  Printf.fprintf oc "constTerm\n";
  Printf.fprintf oc "appTerm\n";
  Printf.fprintf oc "appTerm\n"


let rec print_te ctx oc = function
  | ForallP(var,te) ->
    let ctx' = add_ty_var ctx var in
    print_te ctx' oc te
  | Te(_te) -> print__te ctx oc _te

let judgment_of = function
  | Assume(j,_)     -> j
  | Lemma(_,j)      -> j
  | Conv(j,_,_)     -> j
  | ImplE(j,_,_)    -> j
  | ImplI(j,_,_)    -> j
  | ForallE(j,_,_)  -> j
  | ForallI(j,_,_)  -> j
  | ForallPE(j,_,_) -> j
  | ForallPI(j,_,_) -> j

let rec print_proof oc = function
  | Assume(j,var) -> failwith "todo"
  | Lemma(name,j) -> failwith "todo"
  | Conv(_,proof,_) -> failwith "todo"
  | ImplE(_,left,right) -> failwith "todo"
  | ImplI(j,proof,var) -> failwith "todo"
  | ForallE(_,proof,_te) -> failwith "todo"
  | ForallI(j,proof,var) -> failwith "todo"
  | ForallPE(_,proof,_ty) -> failwith "todo"
  | ForallPI(_,proof,var) -> failwith "todo"

let rec print_item oc = function
  | Parameter(name,ty) -> ()
  | Definition(name,ty,te) ->
    print_te empty_env oc te;
    print_name oc name;
    Printf.fprintf oc "defineConst\n";
    Printf.fprintf oc "pop\n";
    Printf.fprintf oc "pop\n"
  | Axiom(name,te) ->
    print_te empty_env oc te;
    Printf.fprintf oc "nil\n";
    print_te empty_env oc te;
    Printf.fprintf oc "nil\n";
    Printf.fprintf oc "axiom\n";
    Printf.fprintf oc "thm\n";
  | Theorem(name,te,proof) -> print_item oc (Axiom(name,te))
  | TyOpDef(tyop,arity) -> ()

let print_ast oc file ast =
  List.iter (print_item oc) ast.items
