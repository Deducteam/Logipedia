type export = [`Coq | `Matita | `Pvs | `Latex | `Ascii | `OpenTheory ]

module type E =
sig
  val export_ast : Format.formatter -> Ast.ast -> unit
end

type systems_infos =
  {
    extension : string
  }
