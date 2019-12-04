(** Define functions to interact with intermediate logics called
    "middleware". *)

open Kernel

(** Specification of a middleware. *)
module type S = sig
  type tx
  (** Taxons of the logic. A taxon brings some meta information on an entry,
      e.g. distinguishing 'theorems' from 'lemmas'. *)

  exception IllTaxon
  (** Exception raised when reading an ill formed taxon. *)

  val theory : string
  (** Name of the theory. *)

  val tx_of_def : Term.term option -> Term.term -> tx
  (** [tx_of_def t u] returns a taxon of a term [t] with annotation [u] given
      that [t] and [u] come from a definition. *)

  val tx_of_decl : Term.term -> tx
  (** [tx_of_def t] returns a taxon of a term [t] given that [t] comes
      from a declaration. *)

  val string_of_tx : ?short:bool -> tx -> string
  (** [string_of_tx ?(short=false) tx] makes a long or [short] string out of
      taxon [tx].
      Ensure that [string_of_tx (tx_of_string t) = t]. *)

  val tx_of_string : string -> tx
  (** [tx_of_string s] converts string [s] to a taxon. Ensure that
      [string_of_tx (tx_of_string t) = t].

      @raise IllTaxon if the string is not a taxon. *)

  val is_axiomatic : tx -> bool
  (** [is_axiomatic t] is true if taxon [t] should be considered as an
      axiom. *)

  val fields_of_def : tx -> 'a option -> 'a -> 'a * 'a option
  (** [fields_of_def tx teo te] allows to remap the values [teo] and [te] which
      are the two last arguments of a Dedukti definition with taxon [tx] into
      a couple [(ppt, ppto)], with
      - [ppt] used as {!field:Json_types.item.ppt_term} and
      - [ppto] as {!field:Json_types.item.ppt_term_opt}.
      The meaning of [(ppt, ppto)] is given by {!val:label}. *)

  val label : tx -> string * string option
  (** [label tx] returns labels for the fields {!Json_types.item.term} and
      {!Json_types.item.term_opt}. *)

  val string_of_item : Parsing.Entry.entry -> Systems.system -> string
  (** [string_of_item item system] returns a string representation of
      [item] in the export system [system]. This will be printed on
      the website in the export fields. *)
end

module Dummy : S
(** A dummy logic. *)
