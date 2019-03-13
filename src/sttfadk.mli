(** This module give an interface for Dedukti symbols use to represent the STTforall logic. *)

(** Name of the file implementing the STTforall logic. *)
val sttfa_module           : Basic.mident

val sttfa_arrow            : Basic.ident
val sttfa_eps              : Basic.ident
val sttfa_eta              : Basic.ident
val sttfa_etap             : Basic.ident
val sttfa_forall           : Basic.ident
val sttfa_forall_kind_prop : Basic.ident
val sttfa_forall_kind_type : Basic.ident
val sttfa_impl             : Basic.ident
val sttfa_leibniz          : Basic.ident
val sttfa_p                : Basic.ident
val sttfa_prop             : Basic.ident
val sttfa_ptype            : Basic.ident
val sttfa_type             : Basic.ident

val is_sttfa_const : Basic.ident  -> Term.term -> bool
