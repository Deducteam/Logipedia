(** Uses the make facility to solve dependency issues. *)
(* TODO:
   - minimise number of parsing of input file,
   - remove memoization at the level of [doc_of_entries], memoize here,
   - set correctly cl options,
   - avoid rebuilds... *)

open Json
module K = Kernel
module Denv = Api.Env.Default

module E = Api.Env.Make(Kernel.Reduction.Default)
module ErrorHandler = Api.Errors.Make(E)

let deps_of_md : in_channel -> K.Basic.mident -> K.Basic.mident list =
  fun ichan md ->
  Api.Dep.compute_ideps := false;
  let entries = Parsing.Parser.Parse_channel.parse md ichan in
  begin try Api.Dep.make md entries
    with e -> ErrorHandler.graceful_fail None e
  end;
  let deps = Hashtbl.find Api.Dep.deps md in
  Api.Dep.MDepSet.to_seq deps.deps |> Seq.map fst |> List.of_seq

(** {1 General definitions} *)

type key =
  | JsMd of K.Basic.mident
  | DkMd of K.Basic.mident

(** {1 GNU Make like behaviour} *)

(** Old style make rules. *)
type ('k, 'v) rulem = { m_creates : 'k
                      ; m_depends : 'k list
                      ; m_action : 'v list -> 'v }

let rec buildm : ('k, 'v) rulem list -> 'k -> 'v = fun rules target ->
  let rule = List.find (fun r -> r.m_creates = target) rules in
  rule.m_action (List.map (buildm rules) rule.m_depends)

(* Unit because we use the result in the [doc_of_entries]
   function. This should be fixed: [doc_of_entries] should take other
   document in the json type as arguments. *)
(** [make_doc_rulem JsExp fmt file] *)
let make_doc_rulem : (module Compile.S) -> Format.formatter -> string ->
  (key, unit) rulem = fun (module JsExp) fmt file ->
  let md = Denv.init file in
  let input = open_in file in
  let m_depends = List.map (fun x -> DkMd(x)) (deps_of_md input md) in
  close_in input;
  let m_action _ =
    (* Argument discarded as we previously built jsons in
       [doc_of_entries]. Memoization can be done at the level of make,
       and not [doc_of_entries]. *)
    let input = open_in file in
    let entries = Parsing.Parser.Parse_channel.parse md input in
    close_in input;
    JsExp.print_document fmt
      (JsExp.doc_of_entries md entries)
  in
  {m_creates=JsMd(md); m_depends; m_action}

(** {1 Shake like behaviour} *)

type ('k, 'v) action =
  | Finished of 'v
  | Depends  of 'k * ('v -> ('k, 'v) action)
  (** A dependence on a key along with the way to use the value from
      this dependency. *)

(** Dynamic rules. *)
type ('k, 'v) rule = 'k * ('k, 'v) action

let rec build : ('k, 'v) rule list -> 'k -> 'v = fun rules target ->
  let rec run = function
    | Finished(v)  -> v
    | Depends(d,a) -> run (a (build rules d))
  in
  let action = snd (List.find (fun r -> (fst r) = target) rules) in
  run action
