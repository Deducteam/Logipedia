open Ast

let print_name oc (md,id) = Printf.fprintf oc "%s.%s" md id

let print_item oc item =
  match item with
  | Parameter(name,_)    -> Printf.fprintf oc "p, %a\n" print_name name
  | Definition(name,_,_) -> Printf.fprintf oc "d, %a\n" print_name name
  | Axiom(name,_)        -> Printf.fprintf oc "a, %a\n" print_name name
  | Theorem(name,_,_)    -> Printf.fprintf oc "t, %a\n" print_name name
  | TyOpDef(name,_)      -> Printf.fprintf oc "o, %a\n" print_name name

let print_ast oc string ast =
  List.iter (print_item oc) ast.items
