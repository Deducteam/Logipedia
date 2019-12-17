open Extras

(** Manipulated result, to be stored in the database. *)
type ('key, 'value) resultm =
  { r_created : 'key
  (** Key of the value to find it back in the database. *)
  ; r_value : 'value
  (** The computed value. *)
  ; r_built : int
  (** Building timestamp. *) }

type ('key, 'value) rulem =
  { m_creates : 'key
  ; m_depends : 'key list
  ; m_action : 'value list -> 'value }

(** [pp_rulse fmt rules] pretty prints rules [rules] to formatter
    [fmt] using function [pp_key] to pretty print the keys. *)
let pp_rulems : 'key pp -> ('key, 'value) rulem list pp =
  fun pp_key fmt rules ->
  let pp_sep = Format.pp_print_newline in
  let pp_rule : ('key, 'value) rulem pp = fun fmt rule ->
    let pp_sep = Format.pp_print_space in
    let pp_keys : ('key list) pp = fun fmt keys ->
      Format.pp_print_list ~pp_sep pp_key fmt keys
    in
    Format.fprintf fmt "@[%a:@ %a@]"
      pp_key rule.m_creates
      pp_keys rule.m_depends
  in
  Format.pp_print_list ~pp_sep pp_rule fmt rules

(** [skipm old ask rule] returns true if rule [rule] does not need
    to be run. Result [old] is the result from a previous run and
    [ask] a way to retrieve results from keys (to get results of
    dependencies). *)
let skipm : ('key, 'value) resultm -> ('key -> ('key, 'value) resultm) ->
  ('key, 'value) rulem -> bool = fun old ask rule ->
  let f x = (ask x).r_built <= old.r_built in
  List.for_all f rule.m_depends

let buildm (type key): key eq -> (key, 'value) rulem list -> key ->
  ('value, key) result = fun key_eq ->
  (* Locally abstract type for the local exception. *)
  (* Counts build processes to timestamp builds. *)
  let time : int ref = ref 0 in
  (* Save previous results in the database. *)
  let module Db = Hashtbl.Make(struct
      type t = key
      let equal = key_eq
      let hash = Stdlib.Hashtbl.hash
    end)
  in
  let database : (key, 'value) resultm Db.t = Db.create 19 in
  (* [ask key] returns the pre-computed result for key [key] or
     @raise Not_found. *)
  let ask : key -> (key, 'value) resultm = fun target ->
    Db.find database target
  in
  (* [store key value] stores value [value] as computed from key
     [key]. *)
  let store : key -> 'value -> unit = fun target value ->
    Db.replace database target
      {r_created=target; r_value=value; r_built=(!time)}
  in
  fun rules target ->
    let exception NoRule of key in
    let rec buildm : key -> 'value = fun target ->
      incr time;
      let rule =
        try List.find (fun r -> key_eq r.m_creates target) rules
        with Not_found -> raise (NoRule(target))
      in
      match try Some(ask target) with Not_found -> None with
      | Some(old) when skipm old ask rule -> old.r_value
      | _                                 ->
        let value = rule.m_action (List.map buildm rule.m_depends) in
        store target value;
        value
    in
    try Ok(buildm target)
    with NoRule(t) -> Error(t)

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
