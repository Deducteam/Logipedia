(** [dep_of_entry md e] compute the direct dependencies of [e] which are not part of [md]. *)
val dep_of_entry : Basic.mident -> Entry.entry -> Ast.QSet.t
