val item_of_entry : Taxonomy.Sttfa.t Extras.Str2Map.t -> Basic.mident
  -> Entry.entry -> Json_types.item option
(** [item_of_entry txs md e] transforms a Dedukti entry [e] coming
    from module [md] into a json item using computed taxons from
    [txs]. *)

val print_document : Format.formatter -> Json_types.document -> unit
(** [print_document fmt doc] pretty prints document [doc] using
    formatter [fmt].  The output is a JSON file. *)
