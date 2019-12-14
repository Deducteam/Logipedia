type 'a eq = 'a -> 'a -> bool

(** {1 Classic make behaviour} *)

(** Type of a rule. The type ['k] is the type of keys and ['v] of values. *)
type ('k, 'v) rulem =
  { m_creates : 'k
  (** Key of the created element. *)
  ; m_depends : 'k list
  (** Key of elements on which the created element depends. *)
  ; m_action : 'v list -> 'v
  (** What to do with the values of the dependencies to create the value of the
      target. *) }

(** [buildm key_eq rules target] builds target [target] thanks to rules [rules]
    using function [key_eq] to find the appropriate rule. *)
let rec buildm : 'k eq -> ('k, 'v) rulem list -> 'k -> 'v =
  fun key_eq rules target ->
  let rule = List.find (fun r -> key_eq r.m_creates target) rules in
  rule.m_action (List.map (buildm key_eq rules) rule.m_depends)

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
let rec build : 'k eq -> ('k, 'v) rule list -> 'k -> 'v =
  fun key_eq rules target ->
  let rec run = function
    | Finished(v)  -> v
    | Depends(d,a) -> run (a (build key_eq rules d))
  in
  rules |> List.find (fun r -> key_eq (fst r) target) |> snd |> run
