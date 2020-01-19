open Console
open Extras

(** Define a logger. *)
let log_build = new_logger "buil"
let log_build = log_build.logger

let log_rule = new_logger "rule"

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

  type ('k, 'v) generator = ('k -> ('k, 'v) rule option)

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

  let target : 'key -> ('key, _) rule = fun k ->
    {m_creates=k; m_depends=[]; m_action=fun _ -> failwith "no action set"}

  let depends : 'key -> ('key, 'v) rule -> ('key, 'v) rule = fun dep r ->
    {r with m_depends = dep :: r.m_depends}

  let (+<) : ('key, 'v) rule -> 'key -> ('key, 'v) rule = fun  dep t ->
    depends t dep

  let assemble : ('v list -> 'v) -> ('k, _) rule -> ('k, 'v) rule =
    fun m_action r -> {r with m_action}

  let ( +> ) dep f = assemble f dep

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

  let build (type k): key_eq:k eq -> string -> valid_stored:(k -> 'v -> bool) ->
    ?generators:((k, 'v) generator list) -> (k, 'v) rule list -> k ->
    ('v, k) result = fun ~key_eq dbfile ~valid_stored ->
    let dbfile = Filename.(dbfile <.> "lpdb") in
    (* Counts build processes to timestamp builds. *)
    let time : int ref = ref 0 in
    (* Module of the database *)
    let module Db = Hashtbl.Make(struct
        type t = k
        let equal = key_eq
        let hash = Stdlib.Hashtbl.hash
      end)
    in
    (* Global database file. *)
    let database : (k, 'v) build_res Db.t =
      if Sys.file_exists dbfile then
        let inchan = open_in dbfile in
        log_build ~lvl:2 "loading [%s]" dbfile;
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
      log_build ~lvl:2 "saving to [%s]" dbfile;
      Marshal.to_channel ochan database [Marshal.Closures];
      close_out ochan
    in
    at_exit save_db;
    (* [ask key] returns the pre-computed result for key [key] in the database
       or @raise Not_found. *)
    let ask : k -> (k, 'v) build_res = fun target ->
      Db.find database target
    in
    (* [store key value] stores value [value] as computed from key [key]. *)
    let store : k -> 'v -> unit = fun target value ->
      Db.replace database target
        {r_created=target; r_value=value; r_built=(!time)}
    in
    (* The build algorithm. *)
    fun ?(generators=[]) rules target ->
      let exception NoRule of k in
      let rec build : k -> 'v = fun target ->
        incr time;
        let rule =
          try List.find (fun r -> key_eq r.m_creates target) rules
          with Not_found ->
            let rec trygen = function
              | []   -> raise (NoRule(target))
              | g::t ->
                match g target with
                | Some(r) -> r
                | None    -> trygen t
            in
            trygen generators
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
