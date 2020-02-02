(** Define functions to interact with intermediate logics called
    "middleware". Middlewares are used by the json exporter to provide
    additional information on items.

    A middleware is defined by a module (e.g. {!module:MidSttfa})
    implementing the signature {!val:S} and exposed in this file. *)
open Core
open Extras
open Console
module T = Kernel.Term

(** Specification of a middleware. *)
module type S = sig
  type tx
  (** Taxons of the logic. A taxon brings some meta information on an entry,
      e.g. distinguishing 'theorems' from 'lemmas'. *)

  type item
  (** Items of the logic. Can be directly Dedukti entries, or a separate ast. *)

  exception IllTaxon
  (** Exception raised when reading an ill formed taxon. *)

  val theory : string
  (** Name of the theory. *)

  val encoding : DkTools.Mident.t list
  (** List of modules that encode the theory. *)

  val tx_of_entry : Parsing.Entry.entry -> tx option
  (** [tx_of_entry e] returns a taxon of an entry [e], if it is
      taxon-associated entry (ie. a declaration, not a command). *)

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

  val item_of_entry : DkTools.Mident.t -> DkTools.entry -> item
  (** [item_of_entry md entry] returns an item of the logic given an appropriate
      Dedukti entry [entry] of module [md]. *)

  (** {b NOTE} The export section of the website will
      - Display the output of {!val:string_of_item} for all statements
      - Offer to download the whole file processed with {!val:get_exporter}. *)

  val string_of_item : Systems.t -> item -> string
  (** [string_of_item system item] returns a string representation
      of [item] in the export system [system].
      This will be printed on the website in the export fields. *)

  val get_exporter : Systems.t -> (module Export.S)
  (** [get_exporter system] Generate an Exporter module to the given system.
      This allows to handle export through the Makefile system. *)
end

(** A dummy middleware. *)
module Dummy : S =
struct
  type tx = unit
  type item = unit
  let theory = "dummy"
  let encoding = []
  exception IllTaxon
  let tx_of_entry _ = None
  let string_of_tx ?short:_ _ = "dummy"
  let tx_of_string _ = ()
  let is_axiomatic _ = false
  let fields_of_def _ _ t = t,None
  let label _ = "dummy",None
  let item_of_entry _ _ = ()
  let string_of_item _ _ = "dummy"
  let get_exporter _ = assert false
end

(** {1 Bindings of available middlewares} *)

(** Calculus of Universe Polymorphic Inductive Constructions with Eta and
    Fixpoints. *)
module Cupicef : S = MidCupicef

(** Calculus of Template Polymorphic Inductive Constructions with Eta and
    Fixpoints. *)
module Ctpicef : S = MidCtpicef

(** Simple Type Theory ∀. *)
module Sttfa : S = MidSttfa

(** An association list mapping strings to the modules. Along with
    {!val:of_string}, it defines by which identifier any middleware is
    available. *)
let spec : (string * (module S)) list =
  [ ( "dummy"  , (module Dummy  ) )
  ; ( "sttfa"  , (module Sttfa  ) )
  ; ( "cupicef", (module Cupicef) )
  ; ( "ctpicef", (module Ctpicef) )  ]

(** [of_string s] returns the middleware associated to string [s]. *)
let of_string : string -> (module S) = fun s ->
  try List.assoc (String.lowercase_ascii s) spec
  with Not_found -> exit_with "Middleware not found: %s" s

(** Command line options to select middleware. It appears in this
    module rather than {!module:Console.Cli} to avoid cyclic
    dependencies. *)
let options : Cli.t list =
  let available_mid = List.map fst spec |> String.concat ", " in
  [ ( "-m"
    , Arg.Set_string Cli.middleware
    , Format.sprintf " Middleware to use; one of %s" available_mid ) ]
