open Extras

exception NoRuleToMakeTarget

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
    using function [key_eq] to find the appropriate rule. Returns [Ok(v)] if the
    value is computed successfully or [Error(t)] if there is no rule to build
    target [t]. *)
let buildm : 'k eq -> ('k, 'v) rulem list -> 'k -> ('v, 'k) result =
  fun (type k) (key_eq: k eq) (rules: (k, 'v) rulem list) (target: k) ->
  (* Locally abstract type to be able to create local exception *)
  let exception NoRule of k in
  let rec buildm : k -> 'v = fun target ->
    let rule =
      try
        List.find (fun r -> key_eq r.m_creates target) rules
      with Not_found -> raise (NoRule(target))
    in
    rule.m_action (List.map buildm rule.m_depends)
  in
  try Ok(buildm target)
  with NoRule(t) -> Error(t)

(** [pp_rulse pp_key fmt rules] pretty prints rules [rules] to formatter [fmt]
    using function [pp_key] to pretty print the keys. *)
let pp_rules : 'k pp -> ('k, _) rulem list pp = fun pp_key fmt rules ->
  let pp_sep = Format.pp_print_newline in
  let pp_rule : ('k, _) rulem pp = fun fmt rule ->
    let pp_sep = Format.pp_print_space in
    let pp_keys : ('k list) pp = fun fmt keys ->
      Format.pp_print_list ~pp_sep pp_key fmt keys
    in
    Format.fprintf fmt "@[%a:@ %a@]" pp_key rule.m_creates pp_keys rule.m_depends
  in
  Format.pp_print_list ~pp_sep pp_rule fmt rules

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
