module ItemsBuilder : Api.Processor.S with type t = Ast.item list

module DepBuilder : Api.Processor.S with type t = Ast.QSet.t
