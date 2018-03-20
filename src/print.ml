open Ast

let print_item : out_channel -> item -> unit = fun oc it ->
  match it with
  | _ -> Printf.fprintf oc "Item (TODO)\n%!";

let print_ast : out_channel -> ast -> unit = fun oc ast ->
  List.iter (print_item oc) ast
