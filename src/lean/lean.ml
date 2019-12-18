open Core.Extras
open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.S = Middleware.Sttfa

let export : Ast.t pp = fun fmt ast ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Lean in
  M.print_ast fmt ast
