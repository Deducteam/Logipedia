module D = Core.Deps
open Ast

let sys = "matita"

let sanitize id =
  if id = "return" || id = "and" then id ^ "_"
  else if String.sub id 0 1 = "_" then "j"^id else id

let print_var oc id =
  Format.fprintf oc "%s" (sanitize id)

let rec print_list sep pp oc = function
  | [] -> Format.fprintf oc ""
  | [x] -> Format.fprintf oc "(%a)" pp x
  | x::t -> Format.fprintf oc "(%a)%s%a" pp x sep (print_list sep pp) t

let print_dep oc dep =
  Format.fprintf oc "include \"%s.ma\".\n" dep

let print_name oc (_,id) =
  let id = sanitize id in
  Format.fprintf oc "%s" id

let rec print_arity oc arity =
  if arity = 0 then
    Format.fprintf oc "Type[0]"
  else
    Format.fprintf oc "Type[0] -> %a" print_arity (arity-1)

let rec print__ty oc = function
  | TyVar(var) ->
    Format.fprintf oc "%a" print_var var
  | Arrow(_tyl,_tyr) ->
    Format.fprintf oc "%a -> %a" print__ty_wp _tyl print__ty _tyr
  | TyOp(tyOp, []) ->
    Format.fprintf oc "%a" print_name tyOp
  | TyOp(tyOp, _tys) ->
    Format.fprintf oc "%a %a" print_name tyOp (print_list " " print__ty) _tys
  | Prop -> Format.fprintf oc "Prop"

and print__ty_wp fmt _ty =
  match _ty with
  | TyVar _
  | Prop
  | TyOp _ -> print__ty fmt _ty
  | Arrow _ -> Format.fprintf fmt "(%a)" print__ty _ty

let rec print_ty oc = function
  | ForallK(var, ty) ->
    Format.fprintf oc "\\forall %a : Type[0] . %a" print_var var print_ty ty
  | Ty(_ty) -> print__ty oc _ty

let rec print__te oc = function
  | TeVar(var) ->
    Format.fprintf oc "%a" print_var var
  | Abs(var,_ty,_te) ->
    Format.fprintf oc "\\lambda %a : %a. %a" print_var var print__ty _ty print__te _te
  | App(Abs _ as _tel,_ter) -> (* Some proofs are not in beta normal form *)
    Format.fprintf oc "((%a) %a)" print__te _tel print__te_wp _ter
  | App(_tel,_ter) ->
    Format.fprintf oc "%a %a" print__te _tel print__te_wp _ter
  | Forall(var,_ty,_te) ->
    Format.fprintf oc "\\forall (%a:%a). %a" print_var var print__ty _ty print__te _te
  | Impl(_tel,_ter) ->
    Format.fprintf oc "(%a) -> %a" print__te _tel print__te _ter
  | AbsTy(var, _te) ->
    Format.fprintf oc "\\lambda %a : Type[0]. %a" print_var var print__te _te
  | Cst(cst, _tys) ->
    Format.fprintf oc "(%a) %a" print_name cst (print_list " " print__ty) _tys

and print__te_wp fmt _te =
  match _te with
  | TeVar _
  | Cst(_,[]) -> Format.fprintf fmt "%a" print__te _te
  | _ -> Format.fprintf fmt "(%a)" print__te _te

let rec print_te oc = function
  | ForallP(var,te) ->
    Format.fprintf oc "\\forall %a. %a" print_var var print_te te
  | Te(_te) -> print__te oc _te

let rec print_proof oc = function
  | Assume(_,var) -> Format.fprintf oc "%a" print_var var
  | Lemma(name,_) -> Format.fprintf oc "%a" print_name name
  | Conv(_,proof,_) -> Format.fprintf oc "%a" print_proof proof
  | ImplE(_,left,right) -> Format.fprintf oc "(%a) (%a)" print_proof left print_proof right
  | ImplI(_,proof,var) ->
    let j' = judgment_of proof in
    let _,_te = TeSet.choose (TeSet.filter (fun (x,_) -> if x = var then true else false) j'.hyp) in
    Format.fprintf oc "\\lambda %a : %a. (%a)" print_var var print__te _te print_proof proof
  | ForallE(_,proof,_te) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__te _te
  | ForallI(_,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_ty) -> if x = var then true else false) j'.te in
    Format.fprintf oc "\\lambda %a : %a. %a" print_var var print__ty _ty print_proof proof
  | ForallPE(_,proof,_ty) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__ty _ty
  | ForallPI(_,proof,var) ->
    Format.fprintf oc "\\lambda %a : Type[0]. %a" print_var var print_proof proof

let print_item oc = function
  | Parameter(name,ty) ->
    Format.fprintf oc "axiom %a : %a.@." print_name name print_ty ty
  | Definition(name,ty,te) ->
    Format.fprintf oc "definition %a : %a := %a.@." print_name name print_ty ty print_te te
  | Axiom(name,te) ->
    Format.fprintf oc "axiom %a : %a.@." print_name name print_te te
  | Theorem(name,te,proof) ->
    Format.fprintf oc "definition %a : %a := %a.@." print_name name print_te te print_proof proof
  | TypeDecl(tyop,arity) ->
    Format.fprintf oc "axiom %a : %a.@." print_name tyop print_arity arity
  | TypeDef _ -> failwith "[Matita] Type definitions not handled right now"

let print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit = fun fmt ?mdeps:_ ast ->
  print_dep fmt "basics/pts";
  D.QSet.iter (print_dep fmt) ast.dep;
  List.iter (print_item fmt) ast.items

(* FIXME: Probably there is something to do in case of a conflict name *)
let print_meta_ast fmt meta_ast =
  let print_ast fmt ast =
    D.QSet.iter (print_dep fmt) ast.dep;
    List.iter (print_item fmt) ast.items
  in
  print_dep fmt "basics/pts";
  List.iter (print_ast fmt) meta_ast

let to_string fmt = Format.asprintf "%a" fmt

let string_of_item = function
  | Parameter((_,id),ty) ->
    Format.asprintf "axiom %s : %a" id print_ty ty
  | Definition((_,id),ty,te) ->
    Format.asprintf "definition %s : %a := %a" id print_ty ty print_te te
  | Axiom((_,id),te) ->
    Format.asprintf "axiom %s : %a" id print_te te
  | Theorem((_,id),te,_) ->
    Format.asprintf "theorem %s : %a." id print_te te
  | TypeDecl((_,id),arity) ->
    Format.asprintf "axiom %s : %a" id print_arity arity
  | TypeDef _ -> failwith "[Matita] Type definitions not handled right now"
