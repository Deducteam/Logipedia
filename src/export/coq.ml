open Ast
open Printf

let cur_md = ref ""

let sanitize id =
  if id = "return" then id ^ "_" else id

let print_var oc id =
  Format.fprintf oc "%s" (sanitize id)

let rec print_list sep pp oc = function
  | [] -> Format.fprintf oc ""
  | [x] -> Format.fprintf oc "(%a)" pp x
  | x::t -> Format.fprintf oc "(%a)%s%a" pp x sep (print_list sep pp) t

let print_dep oc dep =
  if dep = "sttfa" then ()
  else
    Format.fprintf oc "Require Import %s.\n" dep

let print_name oc (md,id) =
  let id = sanitize id in
  if !cur_md = md then
    Format.fprintf oc "%s" id
  else
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
  | TyOp(tyOp, _tys) ->
    Format.fprintf oc "(%a) %a" print_name tyOp (print_list " " print__ty) _tys
  | Prop -> Format.fprintf oc "Prop"

let rec print_ty oc = function
  | ForallK(var, ty) ->
    Format.fprintf oc "forall (%a:Type), %a" print_var var print_ty ty
  | Ty(_ty) -> print__ty oc _ty

let rec print__te oc = function
  | TeVar(var) ->
    Format.fprintf oc "%a" print_var var
  | Abs(var,_ty,_te) ->
    Format.fprintf oc "fun (%a:%a) => %a" print_var var print__ty _ty print__te _te
  | App(_tel,_ter) ->
    Format.fprintf oc "(%a) (%a)" print__te _tel print__te _ter
  | Forall(var,_ty,_te) ->
    Format.fprintf oc "forall (%a:%a), %a" print_var var print__ty _ty print__te _te
  | Impl(_tel,_ter) ->
    Format.fprintf oc "(%a) -> %a" print__te _tel print__te _ter
  | AbsTy(var, _te) ->
    Format.fprintf oc "fun (%a:Type) => %a" print_var var print__te _te
  | Cst(cst, _tys) ->
    Format.fprintf oc "(%a) %a" print_name cst (print_list " " print__ty) _tys

let rec print_te oc = function
  | ForallP(var,te) ->
    Format.fprintf oc "forall %a, %a" print_var var print_te te
  | Te(_te) -> print__te oc _te

let rec print_proof oc = function
  | Assume(j,var) -> Format.fprintf oc "%a" print_var var
  | Lemma(name,j) -> Format.fprintf oc "%a" print_name name
  | Conv(_,proof,_) -> Format.fprintf oc "%a" print_proof proof
  | ImplE(_,left,right) -> Format.fprintf oc "(%a) (%a)" print_proof left print_proof right
  | ImplI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_te = TeSet.choose (TeSet.filter (fun (x,te) -> if x = var then true else false) j'.hyp) in
    Format.fprintf oc "fun (%a:%a) => (%a)" print_var var print__te _te print_proof proof
  | ForallE(_,proof,_te) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__te _te
  | ForallI(j,proof,var) ->
    let j' = judgment_of proof in
    let _,_ty = List.find (fun (x,_ty) -> if x = var then true else false) j'.te in
    Format.fprintf oc "fun (%a:%a) => %a" print_var var print__ty _ty print_proof proof
  | ForallPE(_,proof,_ty) -> Format.fprintf oc "(%a) (%a)" print_proof proof print__ty _ty
  | ForallPI(_,proof,var) ->
    Format.fprintf oc "fun (%a:Type) => %a" print_var var print_proof proof

let print_item oc = function
  | Parameter(name,ty) ->
    Format.fprintf oc "Parameter %a : %a.\n" print_name name print_ty ty
  | Definition(name,ty,te) ->
    Format.fprintf oc "Definition %a : %a := %a.\n" print_name name print_ty ty print_te te
  | Axiom(name,te) ->
    Format.fprintf oc "Axiom %a : %a.\n" print_name name print_te te
  | Theorem(name,te,proof) ->
    Format.fprintf oc "Definition %a : %a := %a.\n" print_name name print_te te print_proof proof
  | TyOpDef(tyop,arity) ->
    Format.fprintf oc "Parameter %a : %a.\n" print_name tyop print_arity arity

let print_ast oc file ast =
  cur_md := ast.md;
  QSet.iter (print_dep oc) ast.dep;
  List.iter (print_item oc) ast.items

let (_:Thread.t) = Thread.create (fun () ->
  let i = ref 0 in
  while true do Gc.compact(); incr i; if !i mod 100 = 0 then (print_char '.'; flush stdout) done) ()


module P = Mysql.Prepared

let open_bdd () = Mysql.quick_connect ~database:("logipedia") ~user:("walid") ~password:("root") ()

let insert_parameter db (md,id) ty = let insert = P.create db ("INSERT INTO parameters VALUES (null,?,?,?,?)") in
                                            ignore(P.execute insert [|md;id;ty;"2"|]);
                                            P.close insert

let insert_definition db (md,id) ty te = let insert = P.create db ("INSERT INTO definitions VALUES (null,?,?,?,?,?)") in
                                                ignore(P.execute insert [|md;id;ty;te;"2"|]);
                                                P.close insert

let insert_theorem db (md,id) ty te = let insert = P.create db ("INSERT INTO theoremes VALUES (null,?,?,?,?,?)") in
                                              ignore(P.execute insert [|md;id;ty;te;"2"|]);
                                              P.close insert

let insert_axiom db (md,id) te = let insert = P.create db ("INSERT INTO axiomes VALUES (null,?,?,?,?)") in
                                        ignore(P.execute insert [|md;id;te;"2"|]);
                                        P.close insert

let to_string fmt = Format.asprintf "%a" fmt

let print_bdd_item db = function
  | Parameter(name,ty) ->
    insert_parameter db name (to_string print_ty ty)
  | Definition(name,ty,te) ->
    insert_definition db name (to_string print_ty ty) (to_string print_te te)
  | Axiom(name,te) ->
    insert_axiom db name (to_string print_te te)
  | Theorem(name,te,proof) ->
    insert_theorem db name (to_string print_te te) (to_string print_proof proof)
  | TyOpDef(tyop,arity) ->
    insert_parameter db tyop (to_string print_arity arity)

let close_bdd db = Mysql.disconnect db

let print_bdd ast =
  let db = open_bdd () in
  List.iter (print_bdd_item db) ast.items;
  close_bdd db
