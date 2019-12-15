(** All the rules to produce json files with {!module:Core.Build} *)
(* TODO:
   - minimise number of parsing of input file,
   - remove memoization at the level of [doc_of_entries], memoize here,
   - set correctly cl options,
   - avoid rebuilds... *)

open Core.Extras
open Core.Build
module K = Kernel
module Denv = Api.Env.Default
module Dpp = Api.Pp.Default

module E = Api.Env.Make(Kernel.Reduction.Default)
module ErrorHandler = Api.Errors.Make(E)

let deps_of_md : in_channel -> K.Basic.mident -> K.Basic.mident list =
  fun ichan md ->
  Api.Dep.compute_ideps := false;
  let entries = Parsing.Parser.Parse_channel.parse md ichan in
  begin try Api.Dep.make md entries
    with e -> ErrorHandler.graceful_fail None e
  end;
  try
    let deps = Hashtbl.find Api.Dep.deps md in
    Api.Dep.MDepSet.to_seq deps.deps |> Seq.map fst |> List.of_seq
  with Not_found -> assert false

(** {1 General definitions} *)

type key =
  | JsMd of K.Basic.mident
  | DkMd of K.Basic.mident

(** [pp_key fmt k] prints key [k] on format [fmt]. *)
let pp_key : key pp = fun fmt k ->
  match k with
  | DkMd(m) -> Format.fprintf fmt "DkMd(%a)" Dpp.print_mident m
  | JsMd(m) -> Format.fprintf fmt "JsMd(%a)" Dpp.print_mident m

(** [key_eq k l] returns true iff [k] and [l] are equal. *)
let key_eq : key eq = fun k l ->
  match k, l with
  | JsMd(k), JsMd(l)
  | DkMd(k), DkMd(l) ->
    let kstr = K.Basic.string_of_mident k in
    let lstr = K.Basic.string_of_mident l in
    Format.printf "### [%s==%s]... " kstr lstr;
    let r = String.equal kstr lstr in
    if r then Format.printf "Ok@.@\n" else Format.printf "Ko@.@\n";
    r
  (* K.Basic.mident_eq k l *)
  | _                -> false

(* Unit because we use the result in the [doc_of_entries]
   function. This should be fixed: [doc_of_entries] should take other
   document in the json type as arguments. *)
(** [rulem_of_file JsExp ifile odir] creates a rule to build a json file
    [odir/file.json] from input Dedukti file [ifile] using Json exporter
    [JsExp]. [file] is the basename of [ifile] with the suffix [.dk] replaced by
    [.json]. For instance, if [ifile] is [a/b/eq.dk] then [file] is [eq] and the
    created file is then [odir/eq.json]. *)
let rulem_of_file : (module Compile.S) -> string ->
  string -> (key, unit) rulem = fun (module JsExp) infile outdir ->
  let md = Denv.init infile in
  let m_depends =
    let input = open_in infile in
    let ret = List.map (fun x -> DkMd(x)) (deps_of_md input md) in
    close_in input;
    ret
  in
  let m_action _ =
    (* Argument discarded as we previously built jsons in
       [doc_of_entries]. Memoization can be done at the level of make,
       and not [doc_of_entries]. *)
    let entries =
      let input = open_in infile in
      let ret = Parsing.Parser.Parse_channel.parse md input in
      close_in input;
      ret
    in
    let ochan =
      let open Filename in
      let ofile =
        (concat outdir (basename (chop_extension infile))) ^ ".json"
      in
      open_out ofile
    in
    let doc =
      let fmt = Format.formatter_of_out_channel ochan in
      JsExp.print_document fmt (JsExp.doc_of_entries md entries)
    in
    close_out ochan;
    doc
  in
  {m_creates=JsMd(md); m_depends; m_action}
