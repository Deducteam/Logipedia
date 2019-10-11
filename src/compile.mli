module ItemsDepBuilder : Api.Processor.S with
  type t = Ast.item list * Ast.QSet.t
