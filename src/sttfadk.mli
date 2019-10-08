(** This module give an interface for Dedukti symbols use to represent the STTforall logic. *)

(** Name of the file implementing the STTforall logic. *)
val sttfa_module           : Kernel.Basic.mident

val sttfa_arrow            : Kernel.Basic.ident
val sttfa_eps              : Kernel.Basic.ident
val sttfa_eta              : Kernel.Basic.ident
val sttfa_etap             : Kernel.Basic.ident
val sttfa_forall           : Kernel.Basic.ident
val sttfa_forall_kind_prop : Kernel.Basic.ident
val sttfa_forall_kind_type : Kernel.Basic.ident
val sttfa_impl             : Kernel.Basic.ident
val sttfa_leibniz          : Kernel.Basic.ident
val sttfa_p                : Kernel.Basic.ident
val sttfa_prop             : Kernel.Basic.ident
val sttfa_ptype            : Kernel.Basic.ident
val sttfa_type             : Kernel.Basic.ident

val is_sttfa_const : Kernel.Basic.ident  -> Kernel.Term.term -> bool
val is_tyop        : Kernel.Term.term    -> bool
val arity_of_tyop  : Kernel.Term.term    -> int
