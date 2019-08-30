

module type DK =
sig

end

module Dk : DK

module type TYPING =
sig

end

module Typing : TYPING

module type SIGNATURE =
sig

  val item_of_name : Ast.name -> Ast.item

end

module Signature : SIGNATURE
