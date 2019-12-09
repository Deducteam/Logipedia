open Export

module Ast : AST with type t = Sttfa.Ast.ast = Ast

module Mid : Middleware.Middleware_types.S = Middleware.Middleware_sttfa.Sttfa

let export : Ast.t -> Format.formatter -> unit = fun ast fmt ->
  let (module M:Sttfa.Export.E) = Sttfa.Export.of_system `Hollight in
  M.print_ast fmt ast
