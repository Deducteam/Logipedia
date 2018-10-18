let db_name = "logipedia"

let mongo_theory = Mongo.create_local_default db_name "theory"
let mongo_dependencies = Mongo.create_local_default db_name "dependencies"
let mongo_items = Mongo.create_local_default db_name "items"

let of_string = Bson.create_string

let insert collection keyval =
  let doc = List.fold_left (fun doc (key,value) -> Bson.add_element key value doc) Bson.empty keyval in
  Mongo.insert collection [doc]

let insert_theory md id mdDep idDep =
  let values = List.map of_string [md; id; mdDep; idDep] in
  let keys = ["md";"id";"mdDep";"idDep"] in
  let keyval = List.combine keys values in
  insert mongo_theory keyval

let insert_dependency md id mdDep idDep =
  let values = List.map of_string [md; id; mdDep; idDep] in
  let keys = ["md";"id";"mdDep";"idDep"] in
  let keyval = List.combine keys values in
  insert mongo_dependencies keyval

let insert_item md id kind =
  let values = List.map of_string [md; id; kind] in
  let keys = ["md"; "id"; "kind"] in
  let keyval = List.combine keys values in
  insert mongo_items keyval

(*
let of_system = fun sys ->
  match Systems.system_of_string sys with
  | `Pvs        -> 5
  | `Matita     -> 2
  | `OpenTheory -> assert false (* OpenTheory is handled in a different way *)
  | `Lean       -> 4
  | `Coq        -> 3

let sys_id s = string_of_int (of_system s)

let c_constants    = Mongo.create_local_default db_name "constants"
let c_definitions  = Mongo.create_local_default db_name "definitions"
let c_theorems     = Mongo.create_local_default db_name "theorems"
let c_axioms       = Mongo.create_local_default db_name "axioms"
let c_idKind       = Mongo.create_local_default db_name "idKind"
let c_idDep        = Mongo.create_local_default db_name "idDep"
let c_closureIdDep = Mongo.create_local_default db_name "closureIdDep"
let c_mdDep        = Mongo.create_local_default db_name "mdDep"
let c_openTheory   = Mongo.create_local_default db_name "openTheory"

let of_string = Bson.create_string

let insert collection keyval =
  let doc = List.fold_left (fun doc (key,value) -> Bson.add_element key value doc) Bson.empty keyval in
  Mongo.insert collection [doc]

let insert_constant s kw md id ty =
  let values = List.map of_string [kw; md; id; ty; sys_id s] in
  let keys = ["kw";"md";"id";"type";"sys"] in
  let keyval = List.combine keys values in
  insert c_constants keyval

let insert_definition s kw md id ty te =
  let values = List.map of_string [kw; md; id; ty; te; sys_id s] in
  let keys = ["kw";"md";"id";"type";"body"; "sys"] in
  let keyval = List.combine keys values in
  insert c_definitions keyval

let insert_theorem s kw md id te proof =
  let values = List.map of_string [kw; md; id; te; proof; sys_id s] in
  let keys = ["kw";"md"; "id"; "statement"; "proof"; "sys"] in
  let keyval = List.combine keys values in
  insert c_theorems keyval

let insert_axiom s kw md id te =
  let values = List.map of_string [kw; md; id; te; sys_id s] in
  let keys = ["kw"; "md"; "id"; "statement"; "sys"] in
  let keyval = List.combine keys values in
  insert c_axioms keyval

let insert_idDep md id mdDep idDep =
  let values = List.map of_string [md; id; mdDep; idDep] in
  let keys = ["md"; "id"; "mdDep"; "idDep"] in
  let keyval = List.combine keys values in
  insert c_idDep keyval

let insert_mdDep md mdDep bool =
  let values = List.map of_string [md; mdDep;bool] in
  let keys = ["md"; "mdDep"; "isInTransClosure"] in
  let keyval = List.combine keys values in
  insert c_mdDep keyval

let insert_kind md id kind =
  let values = List.map of_string [md; id; kind] in
  let keys = ["md"; "id"; "kind"] in
  let keyval = List.combine keys values in
  insert c_idKind keyval

let insert_closureidDep md id deplist =
  List.iteri (fun order (mdDep,idDep) ->
      let values = List.map of_string [md; id; mdDep; idDep; string_of_int order] in
      let keys = ["md"; "id"; "mdDep"; "idDep"; "order"] in
      let keyval = List.combine keys values in
      insert c_closureIdDep keyval) deplist

let rec insert_openTheory ?(chunk=0) md content =
  let size = String.length content in
  let max = 16000000 in
  if size > max then
    let md' = md^(string_of_int chunk) in
    let content' = String.sub content 0 max in
    let values = List.map of_string [md';content'] in
    let keys = ["md";"content"] in
    let keyval =  List.combine keys values in
    insert c_openTheory keyval;
    insert_openTheory ~chunk:(chunk+1) md (String.sub content max (size-max))
  else
    let md = if chunk <> 0 then md^(string_of_int chunk) else md in
    let values = List.map of_string [md;content] in
    let keys = ["md";"content"] in
    let keyval =  List.combine keys values in
    insert c_openTheory keyval
*)
