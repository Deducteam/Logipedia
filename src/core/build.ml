open Extras

module type KV =
sig
  type key
  type value
  val key_eq : key eq
  val pp_key : key pp
end

module type S =
sig
  type key
  type value
  (** Type of a rule. The type ['k] is the type of keys and ['v] of values. *)
  type rulem =
    { m_creates : key
    (** Key of the created element. *)
    ; m_depends : key list
    (** Key of elements on which the created element depends. *)
    ; m_action : value list -> value
    (** What to do with the values of the dependencies to create the
        value of the target. *) }

  (** [buildm key_eq rules target] builds target [target] thanks to
      rules [rules] using function [key_eq] to find the appropriate
      rule. Returns [Ok(v)] if the value is computed successfully or
      [Error(t)] if there is no rule to build target [t]. *)
  val buildm : rulem list -> key -> (value, key) result

  (** [pp_rulse fmt rules] pretty prints rules [rules] to formatter
      [fmt] using function [pp_key] to pretty print the keys. *)
  val pp_rules : rulem list pp
end

module MakeM : functor (KV: KV) -> S
  with type key = KV.key
   and type value = KV.value =
  functor (KeysVal: KV) ->
  struct
    type key = KeysVal.key
    type value = KeysVal.value
    (** Type of time to avoid making twice same target. Here an [int] as
        the number of build runs. *)
    type b_time = int

    (** {1 Classic make behaviour} *)

    (** Manipulated result. *)
    type resultm =
      { r_created : key
      (** Key of the value to find it back in the database. *)
      ; r_value : value
      (** The computed value. *)
      ; r_built : b_time
      (** Building timestamp. *) }

    (** Type of a rule. The type ['k] is the type of keys and ['v] of values. *)
    type rulem =
      { m_creates : key
      (** Key of the created element. *)
      ; m_depends : key list
      (** Key of elements on which the created element depends. *)
      ; m_action : value list -> value
      (** What to do with the values of the dependencies to create the
          value of the target. *) }

    (** [skipm old ask rule] returns true if rule [rule] does not need
        to be run. Result [old] is the result from a previous run and
        [ask] a way to retrieve results from keys (to get results of
        dependencies). *)
    let skipm : resultm -> (key -> resultm) -> rulem -> bool =
      fun old ask rule ->
      let f x = (ask x).r_built <= old.r_built in
      List.for_all f rule.m_depends

    (** [buildm rules target] builds target [target] thanks to
        rules [rules] using function [k_eq] to find the appropriate
        rule. Returns [Ok(v)] if the value is computed successfully or
        [Error(t)] if there is no rule to build target [t]. *)
    let buildm : rulem list -> key -> (value, key) result =
      (* Locally abstract type for the local exception. *)
      (* Counts build processes to timestamp builds. *)
      let time : b_time ref = ref 0 in
      (* Save previous results in the database. *)
      let database : (key, resultm) Hashtbl.t = Hashtbl.create 19 in
      (* [ask key] returns the pre-computed result for key [key] or
         @raise Not_found. *)
      let ask : key -> resultm = fun target ->
        Hashtbl.find database target
      in
      (* [store key value] stores value [value] as computed from key
         [key]. *)
      let store : key -> value -> unit = fun target value ->
        Hashtbl.replace database target
          {r_created=target; r_value=value; r_built=(!time)}
      in
      fun rules target ->
        let exception NoRule of key in
        let rec buildm : key -> value = fun target ->
          incr time;
          let rule =
            try List.find (fun r -> KeysVal.key_eq r.m_creates target) rules
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

    (** [pp_rulse fmt rules] pretty prints rules [rules] to formatter
        [fmt] using function [pp_key] to pretty print the keys. *)
    let pp_rules : rulem list pp = fun fmt rules ->
      let pp_sep = Format.pp_print_newline in
      let pp_rule : rulem pp = fun fmt rule ->
        let pp_sep = Format.pp_print_space in
        let pp_keys : (key list) pp = fun fmt keys ->
          Format.pp_print_list ~pp_sep KeysVal.pp_key fmt keys
        in
        Format.fprintf fmt "@[%a:@ %a@]"
          KeysVal.pp_key rule.m_creates
          pp_keys rule.m_depends
      in
      Format.pp_print_list ~pp_sep pp_rule fmt rules
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
let rec build : 'k eq -> ('k, 'v) rule list -> 'k -> 'v =
  fun key_eq rules target ->
  let rec run = function
    | Finished(v)  -> v
    | Depends(d,a) -> run (a (build key_eq rules d))
  in
  rules |> List.find (fun r -> key_eq (fst r) target) |> snd |> run
