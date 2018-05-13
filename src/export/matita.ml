open Ast

let sanitize id =
  if id = "return" || id = "and" then id ^ "_"
  else if String.sub id 0 1 = "_" then "j"^id else id

let print_var oc id =
  Printf.fprintf oc "%s" (sanitize id)

let rec print_list sep pp oc = function
  | [] -> Printf.fprintf oc ""
  | [x] -> Printf.fprintf oc "(%a)" pp x
  | x::t -> Printf.fprintf oc "(%a)%s%a" pp x sep (print_list sep pp) t

let print_dep oc dep =
  if dep = "sttfa" then ()
  else
    Printf.fprintf oc "include \"%s.ma\".\n" dep

let print_name oc (md,id) =
  let id = sanitize id in
  Printf.fprintf oc "%s" id

let rec print_arity oc arity =
  if arity = 0 then
    Printf.fprintf oc "Type[0]"
  else
    Printf.fprintf oc "Type[0] -> %a" print_arity (arity-1)

let rec print__ty oc = function
  | TyVar(var) ->
    Printf.fprintf oc "%a" print_var var
  | Arrow(_tyl,_tyr) ->
    Printf.fprintf oc "(%a) -> %a" print__ty _tyl print__ty _tyr
  | TyOp(tyOp, _tys) ->
    Printf.fprintf oc "(%a) %a" print_name tyOp (print_list " " print__ty) _tys
  | Prop -> Printf.fprintf oc "Prop"

let rec print_ty oc = function
  | ForallK(var, ty) ->
    Printf.fprintf oc "\\forall %a : Type[0] . %a" print_var var print_ty ty
  | Ty(_ty) -> print__ty oc _ty

let rec print__te oc = function
  | TeVar(var) ->
    Printf.fprintf oc "%a" print_var var
  | Abs(var,_ty,_te) ->
    Printf.fprintf oc "\\lambda %a : %a. %a" print_var var print__ty _ty print__te _te
  | App(_tel,_ter) ->
    Printf.fprintf oc "(%a) (%a)" print__te _tel print__te _ter
  | Forall(var,_ty,_te) ->
    Printf.fprintf oc "\\forall (%a:%a). %a" print_var var print__ty _ty print__te _te
  | Impl(_tel,_ter) ->
    Printf.fprintf oc "(%a) -> %a" print__te _tel print__te _ter
  | AbsTy(var, _te) ->
    Printf.fprintf oc "\\lambda %a : Type[0]. %a" print_var var print__te _te
  | Cst(cst, _tys) ->
    Printf.fprintf oc "(%a) %a" print_name cst (print_list " " print__ty) _tys

let rec print_te oc = function
  | ForallP(var,te) ->
    Printf.fprintf oc "\\forall %a. %a" print_var var print_te te
  | Te(_te) -> print__te oc _te

let rec print_proof oc = function
  | Assume(j,var) -> Printf.fprintf oc "%a" print_var var
  | Lemma(name,j) -> Printf.fprintf oc "%a" print_name name
  | Conv(_,proof,_) -> Printf.fprintf oc "%a" print_proof proof
  | ImplE(_,left,right) -> Printf.fprintf oc "(%a) (%a)" print_proof left print_proof right
  | ImplI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_te = TeSet.choose (TeSet.filter (fun (x,te) -> if x = var then true else false) j'.hyp) in
    Printf.fprintf oc "\\lambda %a : %a. (%a)" print_var var print__te _te print_proof proof
  | ForallE(_,proof,_te) -> Printf.fprintf oc "(%a) (%a)" print_proof proof print__te _te
  | ForallI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_ty) -> if x = var then true else false) j'.te in
    Printf.fprintf oc "\\lambda %a : %a. %a" print_var var print__ty _ty print_proof proof
  | ForallPE(_,proof,_ty) -> Printf.fprintf oc "(%a) (%a)" print_proof proof print__ty _ty
  | ForallPI(_,proof,var) ->
    Printf.fprintf oc "\\lambda %a : Type[0]. %a" print_var var print_proof proof

let print_item oc = function
  | Parameter(name,ty) ->
    Printf.fprintf oc "axiom %a : %a.\n" print_name name print_ty ty
  | Definition(name,ty,te) ->
    Printf.fprintf oc "definition %a : %a := %a.\n" print_name name print_ty ty print_te te
  | Axiom(name,te) ->
    Printf.fprintf oc "axiom %a : %a.\n" print_name name print_te te
  | Theorem(name,te,proof) ->
    Printf.fprintf oc "definition %a : %a := %a.\n" print_name name print_te te print_proof proof
  | TyOpDef(tyop,arity) ->
    Printf.fprintf oc "axiom %a : %a.\n" print_name tyop print_arity arity

let print_ast oc file ast =
  print_dep oc "basics/pts";
  QSet.iter (print_dep oc) ast.dep;
  List.iter (print_item oc) ast.items
