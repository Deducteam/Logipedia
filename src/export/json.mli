val item_of_entry : Entry.entry -> Json_types.item option
(** [item_of_entry e] transforms a Dedukti entry [e] into a json item. *)

val print_document : Format.formatter -> Json_types.document -> unit
(** [print_document fmt doc] pretty prints document [doc] using
    formatter [fmt].  The output is a JSON file. *)
