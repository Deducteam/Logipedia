(** Makefile emulation. Unlike in Make, there is a {e key} type (represented by
    'k) and a {e value} type (represented by ['v]). Typically, a key depends on
    several other keys: the {e action} uses the values associated to the
    dependencies to create the new value. There are two architecture,
    - the [buildm]/[rulem] which uses static dependencies,
    - the [build]/[rule] which allows to declare dependencies in actions. *)
open Extras
open Console

(** A logger to be used by defined rules. *)
val log_rule : 'a logger

(** Classic make behaviour with precomputed dependencies. *)
module Classic :
sig
  (** Type of a rule. The type ['key] is the type of keys and ['value] of
      values. *)
  type ('key, 'value) rule

  (** [target t] sets [t] as a target for a rule (without action yet). *)
  val target : 'key -> ('key, _) rule

  (** [depends dep rule] sets key [dep] as a dependency for rule [rule]. *)
  val depends : 'key -> ('key, 'v) rule -> ('key, 'v) rule

  (** [dep +< rule] is [depends dep rule]. *)
  val ( +< ) : ('key, 'value) rule -> 'key -> ('key, 'value) rule

  (** [assemble f rule] sets function [f] as the recipe to build the value of
      the target from the values of dependencies set in [rule], and promotes
      [rule] to a usable rule, that is, that can be used with {!val:build}. *)
  val assemble : ('value list -> 'value) -> ('key, 'value) rule ->
    ('key, 'value) rule

  (** [rule +> f] is [assemble f rule]. *)
  val ( +> ) : ('key, _) rule -> ('value list -> 'value) -> ('key, 'value) rule

  (** [pp_rules fmt rules] pretty prints rules [rules] to formatter [fmt] using
      function [pp_key] to pretty print the keys. *)
  val pp_rules : 'key pp -> ('key, 'value) rule list pp

  (** [build ~key_eq ~valid_stored] returns a builder, that is, a function [b]
      such that [b rules target] builds target [target] thanks to rules [rules]
      using function [key_eq] to find the appropriate rule. Returns [Ok(v)] if
      the value is computed successfully or [Error(t)] if there is no rule to
      build target [t]. The function [b] uses a database (stored in folder
      [.logibuild]) to avoid recomputing targets. The function [valid_stored key
      value] returns whether the value [value] of key [key] is up to date in the
      database. *)
  val build : key_eq:'key eq -> valid_stored:('key -> 'value -> bool) ->
    ('key, 'value) rule list -> 'key ->
    ('value, 'key) result
end

(** {1 Shake behaviour} *)

(** The recipe to create a target. *)
type ('k, 'v) action =
  | Finished of 'v
  (** The computed value. *)
  | Depends  of 'k * ('v -> ('k, 'v) action)
  (** A dependency on a key along with the way to use the value computed from
      this dependency. *)

(** Dynamic rules. A couple [(t,r)] is the target [t] along with the recipe
    [r]. *)
type ('k, 'v) rule = 'k * ('k, 'v) action

(** [build key_eq rules target] builds value of target [target] according to
    rules [rules] using function [key_eq] to equate keys. *)
val build : 'k eq -> ('k, 'v) rule list -> 'k -> 'v
