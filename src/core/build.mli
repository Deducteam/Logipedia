(** Makefile emulation. Unlike in Make, there is a {e key} type (represented by
    'k) and a {e value} type (represented by ['v]). Typically, a key depends on
    several other keys: the {e action} uses the values associated to the
    dependencies to create the new value. There are two architecture,
    - the [buildm]/[rulem] which uses static dependencies,
    - the [build]/[rule] which allows to declare dependencies in actions.

    {e NOTE} the current implementation requires a {e unique} key type and a {e
    unique} value type. The chosen solution is to use a polymorphic variant
    type and define an equality on this type. *)

open Extras
open Console

val log_rule : _ logger
(** A logger to be used by defined rules. *)

(** Classic make behaviour with precomputed dependencies. *)
module Classic :
sig
  type ('key, 'value) rule
  (** Type of a rule. The type ['key] is the type of keys and ['value] of
      values. *)

  val target : 'key -> ('key, _) rule
  (** [target t] sets [t] as a target for a rule (without action yet). *)

  val depends : 'key -> ('key, 'v) rule -> ('key, 'v) rule
  (** [depends dep rule] sets key [dep] as a dependency for rule [rule]. *)

  val ( +< ) : ('key, 'value) rule -> 'key -> ('key, 'value) rule
  (** [dep +< rule] is [depends dep rule]. *)

  val assemble : ('value list -> 'value) -> ('key, 'value) rule ->
    ('key, 'value) rule
  (** [assemble f rule] sets function [f] as the recipe to build the value of
      the target from the values of dependencies set in [rule], and promotes
      [rule] to a usable rule, that is, that can be used with {!val:build}. *)

  val ( +> ) : ('key, 'v) rule -> ('v list -> 'v) -> ('key, 'v) rule
  (** [rule +> f] is [assemble f rule]. *)

  val pp_rules : 'key pp -> ('key, 'value) rule list pp
  (** [pp_rules fmt rules] pretty prints rules [rules] to formatter [fmt] using
      function [pp_key] to pretty print the keys. *)

  val build : key_eq:'key eq -> string ->
    valid_stored:('key -> 'value -> bool) ->
    ('key, 'value) rule list -> 'key -> ('value, 'key) result
  (** [build ~key_eq db ~valid_stored] returns a builder, that is, a function
      [b] such that [b rules target] builds target [target] thanks to rules
      [rules] using function [key_eq] to find the appropriate rule. Returns
      [Ok(v)] if the value is computed successfully or [Error(t)] if there is no
      rule to build target [t]. The function [b] uses a database file
      named [db.lpdb] to avoid recomputing targets. The function
      [valid_stored key value] returns whether the value [value] of
      key [key] is up to date in the database. *)
end
