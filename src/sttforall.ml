open Basic

module Dk =
struct

end

module Typing =
struct
end

module Signature =
struct

  let sg = Signature.make ""

  let cst_of : Ast.name -> Basic.name = fun name ->
    Basic.mk_name (Basic.mk_mident (fst name)) (Basic.mk_ident (snd name))

  let item_of_name : Ast.name -> Ast.item = fun name ->
 (* Put that somewhere else *)
      let cst = cst_of name in
      let ty = Signature.get_type sg Basic.dloc cst in
      let entry =
        match Signature.get_rules sg Basic.dloc cst with
        | [] -> Entry.Decl(Basic.dloc, Basic.id cst, Signature.Static,ty)
        | [r] ->
          assert (r.args = []);
          assert (Basic.name_eq r.cst  cst);
          Entry.Def(Basic.dloc, Basic.id cst, false, Some ty, r.rhs)
        | _ -> failwith "Only total definitions are allowed"
      in
      failwith "what the fuck"
      (* Compile.compile_entry (Basic.md cst) entry *)
end
