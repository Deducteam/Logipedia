open Ast

let sys = "lean"

module CType = Compile_type

let theorems = ["refl"; "eq" ; "pred" ; "le" ; "lt" ;
               "decidable_lt" ; "decidable_le" ]

let sanitize id =
  if List.mem id theorems then id^"_"
  else id

let print_var oc id =
  Format.fprintf oc "%s" (sanitize id)

let rec print_list sep pp oc = function
  | [] -> Format.fprintf oc ""
  | [x] -> Format.fprintf oc "(%a)" pp x
  | x::t -> Format.fprintf oc "(%a)%s%a" pp x sep (print_list sep pp) t

let print_dep oc dep =
  if dep = "sttfa" then ()
  else
    Format.fprintf oc "import .%s\n" dep

let print_name oc (md,id) =
  let id = sanitize id in
  Format.fprintf oc "%s.%s" md id

let rec print_arity oc arity =
  if arity = 0 then
    Format.fprintf oc "Type"
  else
    Format.fprintf oc "Type -> %a" print_arity (arity-1)

let rec print__ty oc = function
  | TyVar(var) ->
    Format.fprintf oc "%a" print_var var
  | Arrow(_tyl,_tyr) ->
    Format.fprintf oc "(%a) -> %a" print__ty _tyl print__ty _tyr
  | TyOp(tyOp, []) ->
    Format.fprintf oc "%a" print_name tyOp
  | TyOp(tyOp, _tys) ->
    Format.fprintf oc "(%a) %a" print_name tyOp (print_list " " print__ty) _tys
  | Prop -> Format.fprintf oc "Prop"

and print__ty_wp fmt _ty =
  match _ty with
  | TyVar _
  | Prop
  | TyOp _ -> print__ty fmt _ty
  | Arrow _ -> Format.fprintf fmt "(%a)" print__ty _ty

let rec print_ty oc = function
  | ForallK(var, ty) ->
    Format.fprintf oc "forall (%a : Type) , %a" print_var var print_ty ty
  | Ty(_ty) -> print__ty oc _ty

let rec print__te oc = function
  | TeVar(var) ->
    Format.fprintf oc "%a" print_var var
  | Abs(var,_ty,_te) ->
    Format.fprintf oc "fun (%a : %a) , %a" print_var var print__ty _ty print__te _te
  | App(Abs _ as _tel,_ter) -> (* Some proofs are not in beta normal form *)
    Format.fprintf oc "((%a) %a)" print__te _tel print__te_wp _ter
  | App(_tel,_ter) ->
    Format.fprintf oc "(%a) (%a)" print__te _tel print__te _ter
  | Forall(var,_ty,_te) ->
    Format.fprintf oc "forall (%a:%a) , %a" print_var var print__ty _ty print__te _te
  | Impl(_tel,_ter) ->
    Format.fprintf oc "(%a) -> %a" print__te _tel print__te _ter
  | AbsTy(var, _te) ->
    Format.fprintf oc "fun (%a : Type) , %a" print_var var print__te _te
  | Cst(cst, _tys) ->
    Format.fprintf oc "(%a) %a" print_name cst (print_list " " print__ty) _tys

and print__te_wp fmt _te =
  match _te with
  | TeVar _
  | Cst(_,[]) -> Format.fprintf fmt "%a" print__te _te
  | _ -> Format.fprintf fmt "(%a)" print__te _te

let rec print_te oc = function
  | ForallP(var,te) ->
    Format.fprintf oc "forall (%a : Type) , %a" print_var var print_te te
  | Te(_te) -> print__te oc _te

let rec print_proof oc = function
  | Assume(j,var) -> Format.fprintf oc "%a" print_var var
  | Lemma(name,j) -> Format.fprintf oc "@%a" print_name name
  | Conv(_,proof,_) -> Format.fprintf oc "%a" print_proof proof
  | ImplE(_,left,right) -> Format.fprintf oc "(%a) (%a)" print_proof left print_proof right
  | ImplI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_te = TeSet.choose (TeSet.filter (fun (x,te) -> if x = var then true else false) j'.hyp) in
    Format.fprintf oc "fun (%a : %a) , (%a)" print_var var print__te _te print_proof proof
  | ForallE(_,proof,_te) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__te _te
  | ForallI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_ty) -> if x = var then true else false) j'.te in
    Format.fprintf oc "fun (%a : %a) , %a" print_var var print__ty _ty print_proof proof
  | ForallPE(_,proof,_ty) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__ty _ty
  | ForallPI(_,proof,var) ->
    Format.fprintf oc "fun (%a : Type) , %a" print_var var print_proof proof

let rec is__prop (_ty:_ty) =
  match _ty with
  | Ast.TyVar _ -> false
  | Ast.Arrow (_,_tyr) -> is__prop _tyr
  | Ast.TyOp (_,_) -> false
  | Ast.Prop -> true

let rec is_prop ty =
  match ty with
  | Ty(_ty) -> is__prop _ty
  | ForallK(_,ty) -> is_prop ty

let rec is__computable (_te:_te) =
  match _te with
   | Ast.TeVar _ -> true
   | Ast.Abs (_,_,_te) -> is__computable _te
   | Ast.App (_tel,_ter) -> is__computable _tel && is__computable _ter
   | Ast.Forall (_,_,_te) -> is__computable _te
   | Ast.Impl (_tel,_ter) -> is__computable _tel && is__computable _ter
   | Ast.AbsTy (_,_te) -> is__computable _te
   | Ast.Cst ((cmd,cid),_) ->
     let open Basic in
     let name = mk_name (mk_mident cmd) (mk_ident cid) in
     let ty = Env.get_type dloc name in
     let ty' = CType.compile_wrapped_type Environ.empty_env ty in
     is_prop ty' ||
       not @@ Signature.is_static (Env.get_signature ()) dloc name

let rec is_computable te =
  match te with
  | Te(_te) -> is__computable _te
  | ForallP(_,te) -> is_computable te

let print_item oc = function
  | Parameter(name,ty) ->
    Format.fprintf oc "constant %a : %a.\n" print_name name print_ty ty
  | Definition(name,ty,te) ->
    if is_prop ty || is_computable te then
      Format.fprintf oc "def %a : %a := %a.\n" print_name name print_ty ty print_te te
    else
      Format.fprintf oc "noncomputable def %a : %a := %a.\n" print_name name print_ty ty print_te te
  | Axiom(name,te) ->
    Format.fprintf oc "axiom %a : %a.\n" print_name name print_te te
  | Theorem(name,te,proof) ->
    Format.fprintf oc "theorem %a : %a := %a.\n" print_name name print_te te print_proof proof
  | TyOpDef(tyop,arity) ->
    Format.fprintf oc "axiom %a : %a.\n" print_name tyop print_arity arity

let print_ast oc ast =
  QSet.iter (print_dep oc) ast.dep;
  List.iter (print_item oc) ast.items

let print_meta_ast fmt meta_ast =
  let print_ast fmt ast =
    Format.fprintf fmt "namespace %s\n" ast.md;
    print_ast fmt ast;
    Format.fprintf fmt "end %s\n\n" ast.md
  in
  List.iter (print_ast fmt) meta_ast

let to_string fmt = Format.asprintf "%a" fmt

let print_bdd_item = function
  | Parameter((md,id),ty) ->
    Mongodb.insert_constant sys "constant" md id (to_string print_ty ty)
  | Definition((md,id),ty,te) ->
    if is_prop ty || is_computable te then
      Mongodb.insert_definition sys "def" md id (to_string print_ty ty) (to_string print_te te)
    else
      Mongodb.insert_definition sys "noncomputable def" md id
        (to_string print_ty ty) (to_string print_te te)
  | Axiom((md,id),te) ->
    Mongodb.insert_axiom sys "axiom" md id (to_string print_te te)
  | Theorem((md,id),te,proof) ->
    Mongodb.insert_theorem sys "theorem" md id (to_string print_te te) (to_string print_proof proof)
  | TyOpDef((md,id),arity) ->
    Mongodb.insert_constant sys "axiom" md id (to_string print_arity arity)

let print_bdd ast = List.iter print_bdd_item ast.items;
