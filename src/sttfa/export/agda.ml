module D = Core.Deps
open Ast

let sys = "agda"

let cur_md = ref ""

(* no files in arith_fermat are using them but we still might get new files using them in the future *)
let forbidden_id = ref ["abstract";"consructor";"data";"do";"eta-equality";"field";"forall";"hiding";
                        "import";"in";"inductive";"infix";"infixl";"infixr";"instance";"let";"macro";
                        "module";"mutual";"no-eta-equality";"open";"overlap";"pattern";"postulate";
                        "primitive";"private";"public";"quote";"quoteContext";"quoteGoal";"quoteTerm";
                        "record";"renaming";"rexrite";"Set";"syntax";"tactic";"unquote";"unquoteDecl";
                        "unquoteDef";"using";"variable";"where";"with"]

(* when _ used as wildcard, leave it
  otherwise, change it to :: (- is forbidden because it may lead to inline comments) 
  because i would be interpreted as mix-fix operator *)
let sanitize id =
  if id = "_" 
  then id
  else 
    if List.mem id !forbidden_id
    then 
      "dk^"^id
    else
      let regexp = Str.regexp "_" in
      Str.global_replace regexp "::" id

let print_var oc id =
  Format.fprintf oc "%s" (sanitize id)

let rec print_list sep pp oc = function
  | [] -> Format.fprintf oc ""
  | [x] -> Format.fprintf oc "(%a)" pp x
  | x::t -> Format.fprintf oc "(%a)%s%a" pp x sep (print_list sep pp) t

let print_dep oc dep =
  (* let dep = sanitize dep in *)
  Format.fprintf oc "open import %s\n" dep

let print_name oc (md,id) =
  let id = sanitize id in
  (* let md = sanitize md in *)
  (* sanitize md causes problems, one might need to rename all files with :
     (cd export/agda ; rename _ :: *.agda) *)
  if !cur_md = md then
    Format.fprintf oc "%s" id
  else
    Format.fprintf oc "%s.%s" md id

let rec print_arity oc arity =
  if arity = 0 then
    Format.fprintf oc "Set"
  else
    Format.fprintf oc "Set -> %a" print_arity (arity-1)

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
    (* Format.fprintf oc "forall (%a : Set) -> %a" print_var var print_ty ty *)
    Format.fprintf oc "(%a : Set) -> %a" print_var var print_ty ty
  | Ty(_ty) -> print__ty oc _ty

let rec print__te oc = function
  | TeVar(var) ->
    Format.fprintf oc "%a" print_var var
  | Abs(var,_ty,_te) ->
    Format.fprintf oc "\\(%a : %a) -> %a" print_var var print__ty_wp _ty print__te _te
  | App(Abs _ as _tel,_ter) -> (* Some proofs are not in beta normal form (is it true for Agda ?) *)
    Format.fprintf oc "((%a) %a)" print__te _tel print__te_wp _ter (* not sure *)
  | App(_tel,_ter) ->
    Format.fprintf oc "%a %a" print__te _tel print__te_wp _ter (* not sure *)
  | Forall(var,_ty,_te) ->
    Format.fprintf oc "(%a : %a) -> %a" print_var var print__ty_wp _ty print__te _te
    (* Format.fprintf oc "forall (%a : %a) -> %a" print_var var print__ty_wp _ty print__te _te *)
  | Impl(_tel,_ter) ->
    Format.fprintf oc "%a -> %a" print__te_wp _tel print__te _ter 
  | AbsTy(var, _te) ->
    Format.fprintf oc "\\(%a : Set) -> %a" print_var var print__te _te (* ie id = \(A : Set) -> \(x : A) -> x *)
  | Cst(cst, []) ->
    Format.fprintf oc "%a" print_name cst
  | Cst(cst, _tys) ->
    Format.fprintf oc "%a %a" print_name cst (print_list " " print__ty) _tys

and print__te_wp fmt _te =
  match _te with
  | TeVar _
  | Cst(_,[]) -> Format.fprintf fmt "%a" print__te _te
  | _ -> Format.fprintf fmt "(%a)" print__te _te

let rec print_te oc = function
  | ForallP(var,te) ->
    Format.fprintf oc "(%a : _) -> %a" print_var var print_te te 
    (* Format.fprintf oc "forall %a -> %a" print_var var print_te te *)
  | Te(_te) -> print__te oc _te

let rec print_proof oc = function 
  | Assume(_,var) -> Format.fprintf oc "%a" print_var var
  | Lemma(name,_) -> Format.fprintf oc "%a" print_name name
  | Conv(_,proof,_) -> Format.fprintf oc "%a" print_proof proof
  | ImplE(_,left,right) -> Format.fprintf oc "(%a) (%a)" print_proof left print_proof right
  | ImplI(_,proof,var) -> (* abstraction to indtroduce Impl, needs to extract term from judgment *)
    let j' = judgment_of proof in
    let _,_te = TeSet.choose (TeSet.filter (fun (x,_) -> if x = var then true else false) j'.hyp) in
    Format.fprintf oc "\\(%a : %a) -> (%a)" print_var var print__te _te print_proof proof
  | ForallE(_,proof,_te) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__te _te
  | ForallI(_,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_) -> x = var) j'.te in
    Format.fprintf oc "\\(%a : %a) -> %a" print_var var print__ty _ty print_proof proof
  | ForallPE(_,proof,_ty) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__ty _ty
  | ForallPI(_,proof,var) ->
    Format.fprintf oc "\\(%a : Set) -> %a" print_var var print_proof proof

let print_item oc = function
  | Parameter(name,ty) ->
    Format.fprintf oc "%a : %a@." print_name name print_ty ty
  | Definition(name,ty,te) ->
    Format.fprintf oc "%a : %a@.%a = %a@.@." print_name name print_ty ty print_name name print_te te
  | Axiom (name,te) ->
    Format.fprintf oc "postulate %a : %a@." print_name name print_te te 
  | Theorem(name,te,proof) ->
    Format.fprintf oc "%a : %a@.%a = %a@.@." print_name name print_te te print_name name print_proof proof
  | TypeDecl(tyop,arity) ->
    Format.fprintf oc "%a : %a@." print_name tyop print_arity arity
  | TypeDef (name,_,_) ->
    Format.fprintf oc "-- Type definition (for %a) not handled right now@." print_name name

let print_ast : Format.formatter -> ?mdeps:Ast.mdeps -> Ast.ast -> unit = fun fmt ?mdeps:_ ast ->
  cur_md := ast.md;
  Format.fprintf fmt "module %s where\n" !cur_md;
  D.QSet.iter (print_dep fmt) ast.dep;
  List.iter (print_item fmt) ast.items

(* not used ?
let print_meta_ast fmt meta_ast =
  let print_ast fmt ast =
    Format.fprintf fmt "module %s where\n" ast.md; (* I don't think you need to indent for a top-level module *)
    print_ast fmt ast;
  in
  List.iter (print_ast fmt) meta_ast

let to_string fmt = Format.asprintf "%a" fmt
*)

let string_of_item = function
  | Parameter((_,id),ty) ->
    Format.asprintf "%s : %a" id print_ty ty
  | Definition((_,id),ty,te) ->
    Format.asprintf "%s : %a@.%s = %a" id print_ty ty id print_te te 
  | Axiom((_,id),te) ->
    Format.asprintf "postulate %s : %a" id print_te te
  | Theorem((_,id),te,_) ->
    Format.asprintf "%s : %a" id print_te te
  | TypeDecl((_,id),arity) ->
    Format.asprintf "%s : %a" id print_arity arity
  | TypeDef _ -> failwith "[AGDA] Type definitions not handled right now" 
