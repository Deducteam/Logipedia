(** URI allows a coherent identification of items in Logipedia.

    An URI is of the form [logic:md/name.tx]
    where
    - [logic] is what is expected;
    - [md] is the module specification;
    - [name] is a top level name;
    - [tx] for the taxon, i.e. which kind of object it is. *)

(** The kind of object, e.g. a definition or theorem. *)
type taxon =
  | TxAxm (** Axiom *)
  | TxDef (** Definition *)
  | TxCst (** Constant *)
  | TxThm (** Theorem *)
[@@deriving yojson]

type name
(** Simplest name of an element. *)

type modu
(** Type of module path. *)

type t
(** The uri type. *)

val uri_of_dkid : Basic.mident -> Basic.ident -> Theories.theory -> taxon -> t
(** [uri_of_dkid md id th tx] transforms a Dedukti qualified [md.id]
    (where the [.] separates the module from the symbol name) into a
    uri, with theory [th] and taxon [tx]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt uri] pretty prints [uri] to formatter [fmt].  [uri] is pretty
    printed as [logic:module/name.tx]. *)

val to_string : t -> string
(** [to_string uri] prints uri [uri] as a string [logic:module/name.tx]. *)
