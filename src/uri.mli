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

type name
(** Simplest name of an element. *)

type modu
(** Type of module path. *)

type t
(** The uri type. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt uri] pretty prints [uri] to formatter [fmt].  [uri] is pretty
    printed as [logic:module/name.tx]. *)

val name_of : t -> string
(** [name_of uri] returns the name associated to the uri. *)

val taxon_of : t -> taxon
(** [taxon_of uri] returns the so-called taxon of the [uri]. *)

val modu_of : t -> modu
(** [modu_of uri] returns the module of [uri]. *)
