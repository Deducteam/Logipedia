module B = Basic

module type DK =
sig
end

module Dk : DK =
struct

end

module type TYPING =
sig
end

module Typing : TYPING =
struct
end

module type SIGNATURE =
sig

  val item_of_name : Ast.name -> Ast.item

end

module Signature : SIGNATURE =
struct

  let sg = Signature.make ""

  let cst_of : Ast.name -> Basic.name = fun name ->
    B.mk_name (B.mk_mident (fst name)) (B.mk_ident (snd name))

  let item_of_name : Ast.name -> Ast.item = fun name ->
 (* Put that somewhere else *)
      let cst = cst_of name in
      let ty = Signature.get_type sg B.dloc cst in
      let _ =
        match Signature.get_rules sg B.dloc cst with
        | [] -> Entry.Decl(B.dloc, B.id cst, Signature.Static,ty)
        | [r] ->
          assert (r.args = []);
          assert (B.name_eq r.cst  cst);
          Entry.Def(B.dloc, B.id cst, false, Some ty, r.rhs)
        | _ -> failwith "Only total definitions are allowed"
      in
      failwith "what the fuck"
      (* Compile.compile_entry (Basic.md cst) entry *)
end
