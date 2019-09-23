(** URI allows a coherent identification of items in Logipedia.

    An URI is of the form [logic:md/name.tx]
    where
    - [logic] is what is expected;
    - [md] is the module specification;
    - [name] is a top level name;
    - [tx] for the taxon, i.e. which kind of object it is. *)

type t
(** The uri type. *)

exception IllFormedUri of string
(** Exception raised when trying to convert a string which is not a
    uri into a uri.  The string is the pseudo uri. *)

val to_string : t -> string
(** [to_string uri] prints uri [uri] as a string [logic:module/name.tx]. *)

val of_string : string -> t
(** [of_string s] builds a uri from a string.

    @raise IllFormedUri if [s] is not a valid uri. *)

val uri_of_dkid : Basic.mident -> Basic.ident -> string
  -> string -> t
(** [uri_of_dkid md id th tx] transforms a Dedukti qualified [md.id]
    (where the [.] separates the module from the symbol name) into a
    uri, with theory [th] and taxon [tx]. *)

val name_of_uri : t -> Basic.name
(** [name_of_uri u] returns the Dedukti name of a uri [u]. *)

val ext_of_uri : t -> string
(** [ext_of_uri u] returns the extension of a uri [u].  In our
    context, the extension is a taxonomy (see {!module:Taxonomy}). *)
