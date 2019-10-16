open Kernel
open Parsing
val doc_of_entries : Basic.mident -> Entry.entry list -> Json_types.document
(** [doc_of_entry md e] transforms a list of Dedukti entries [e]
    coming from module [md] into a json document. *)

val print_document : Format.formatter -> Json_types.document -> unit
(** [print_document fmt doc] pretty prints document [doc] using
    formatter [fmt].  The output is a JSON file. *)
