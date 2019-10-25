(** Define functions to interact with intermediate logics called
    "middleware". *)

open Kernel
open Json_types

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
  (** [tx_of_def t u] returns a taxon of a term [t] with annotation [u] given that
      [t] comes from a definition. *)

  val tx_of_decl : Term.term -> tx
  (** [tx_of_def t] returns a taxon of a term [t] given that [t] comes
      from a declaration. *)

  val string_of_tx : ?short:bool -> tx -> string
  (** [string_of_tx ?(short=false) tx] makes a long or [short] string out of
      taxon [tx]. *)

  val tx_of_string : string -> tx
  (** [tx_of_string s] converts string [s] to a taxon.

      @raise IllTaxon if the string is not a taxon. *)

  val is_axiomatic : tx -> bool
  (** [is_axiomatic t] is true if taxon [t] should be considered as an
      axiom. *)

  val fields_of_def : tx -> Ppterm.t option Lazy.t -> Ppterm.t Lazy.t
    -> Ppterm.t * Ppterm.t option
  (** [fields_of_def tx teo te] returns a couple [ppt, ppto] which are the
      ppterms to be put into {!field:Json_types.item.ppt_term} and
      {!field:Json_types.item.ppt_term_opt} coming from [teo] and [te] which
      are from a (Dedukti) definition of taxon [tx]. *)

  val label : tx -> string * string option
  (** [label tx] returns labels for the fields {!Json_types.item.term} and
      {!Json_types.item.term_opt}. *)

end

module Sttfa : S
(** Simple Type Theory For All. *)
