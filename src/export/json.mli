val item_of_entry : Basic.mident -> Entry.entry -> Json_types.item option
(** [item_of_entry md e] transforms a Dedukti entry [e] coming from
    module [md] into a json item. *)

val print_document : Format.formatter -> Json_types.document -> unit
(** [print_document fmt doc] pretty prints document [doc] using
    formatter [fmt].  The output is a JSON file. *)
