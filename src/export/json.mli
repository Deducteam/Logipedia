open Api

module DocumentBuilder : Processor.S with type t = Json_types.document
(** Processor to build a document, to be used with
    {!val:Api.Processor.handle_input}. *)

val print_document : Format.formatter -> Json_types.document -> unit
(** [print_document fmt doc] pretty prints document [doc] using
    formatter [fmt].  The output is a JSON file. *)
