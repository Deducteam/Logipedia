module ItemsBuilder : Api.Processor.S with type t = Ast.item list

module AstBuilder : Api.Processor.S with type t = Ast.ast

module DepBuilder : Api.Processor.S with type t = Ast.QSet.t
