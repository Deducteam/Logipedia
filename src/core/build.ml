open Console
open Extras

(** Directory where metadata are saved. *)
let dbfile = ".logibuild.db"

module Classic =
struct

  (** Manipulated result, to be stored in the database. *)
  type ('key, 'value) build_res =
    { r_created : 'key
    (** Key of the value to find it back in the database. *)
    ; r_value : 'value
    (** The computed value. *)
    ; r_built : int
    (** Building timestamp. *) }

  type ('key, 'value) rule =
    { m_creates : 'key
    ; m_depends : 'key list
    ; m_action : 'value list -> 'value }

  (** [pp_rulse fmt rules] pretty prints rules [rules] to formatter
      [fmt] using function [pp_key] to pretty print the keys. *)
  let pp_rules : 'key pp -> ('key, 'value) rule list pp =
    fun pp_key fmt rules ->
    let pp_sep = Format.pp_print_newline in
    let pp_rule : ('key, 'value) rule pp = fun fmt rule ->
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
  let skip : ('key, 'value) build_res -> ('key -> ('key, 'value) build_res) ->
    ('key, 'value) rule -> bool = fun old ask rule ->
    let skip dep =
      try (ask dep).r_built <= old.r_built
      with Not_found -> false
    in
    List.for_all skip rule.m_depends

  let build (type key): key_eq:key eq -> valid_stored:(key -> 'value -> bool) ->
    (key, 'value) rule list -> key ->
    ('value, key) result = fun ~key_eq ~valid_stored ->
    (* Counts build processes to timestamp builds. *)
    let time : int ref = ref 0 in
    (* Module of the database *)
    let module Db = Hashtbl.Make(struct
        type t = key
        let equal = key_eq
        let hash = Stdlib.Hashtbl.hash
      end)
    in
    (* Global database file. *)
    let database : (key, 'value) build_res Db.t =
      if Sys.file_exists dbfile then
        let inchan = open_in dbfile in
        if !log_enabled then log "[build] loading [%s]" dbfile;
        let db = Marshal.from_channel inchan in
        close_in inchan;
        (* [check k r] removes key [k] from database if result [r] is not
           valid according to function [valid_stored]. *)
        let check k r =
          if valid_stored k r.r_value then Some(r) else None
        in
        (* ... and check its content... *)
        Db.filter_map_inplace check db;
        db
      else Db.create 19
    in
    (* Save database when closing *)
    let save_db () =
      let ochan = open_out dbfile in
      Marshal.to_channel ochan database [];
      close_out ochan
    in
    at_exit save_db;
    (* [ask key] returns the pre-computed result for key [key] in the database or
       @raise Not_found. *)
    let ask : key -> (key, 'value) build_res = fun target ->
      Db.find database target
    in
    (* [store key value] stores value [value] as computed from key [key]. *)
    let store : key -> 'value -> unit = fun target value ->
      Db.replace database target
        {r_created=target; r_value=value; r_built=(!time)}
    in
    (* Definition of the effective build function *)
    fun rules target ->
      (* The build algorithm. *)
      let exception NoRule of key in
      let rec build : key -> 'value = fun target ->
        incr time;
        let rule =
          try List.find (fun r -> key_eq r.m_creates target) rules
          with Not_found -> raise (NoRule(target))
        in
        let compute () =
          let value = rule.m_action (List.map build rule.m_depends) in
          store target value;
          value
        in
        match ask target with
        | old when skip old ask rule -> old.r_value
        | _                          -> compute ()
        | exception Not_found        -> compute ()
      in
      try Ok(build target) with NoRule(t) -> Error(t)
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
