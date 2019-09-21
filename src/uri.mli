(** URI allows a coherent identification of items in Logipedia.

    An URI is of the form [logic:md/name.tx]
    where
    - [logic] is what is expected;
    - [md] is the module specification;
    - [name] is a top level name;
    - [tx] for the taxon, i.e. which kind of object it is. *)

type t
(** The uri type. *)

val to_string : t -> string
(** [to_string uri] prints uri [uri] as a string [logic:module/name.tx]. *)

val uri_of_dkid : Basic.mident -> Basic.ident -> string ->
  Taxonomy.Sttfa.t -> t
(** [uri_of_dkid md id th tx] transforms a Dedukti qualified [md.id]
    (where the [.] separates the module from the symbol name) into a
    uri, with theory [th] and taxon [tx]. *)
