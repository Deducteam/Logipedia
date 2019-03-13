let db_name = "logipedia"

let mongo_theory = Mongo.create_local_default db_name "theory"
let mongo_dependencies = Mongo.create_local_default db_name "dependencies"
let mongo_items = Mongo.create_local_default db_name "items"
let mongo_printing = Mongo.create_local_default db_name "printing"

let of_string = Bson.create_string

let insert collection keyval =
  let doc = List.fold_left (fun doc (key,value) -> Bson.add_element key value doc) Bson.empty keyval in
  Mongo.insert collection [doc]

let insert_theory name nameDep =
  let md,id = name in
  let mdDep, idDep = nameDep in
  let values = List.map of_string [md; id; mdDep; idDep] in
  let keys = ["md";"id";"mdDep";"idDep"] in
  let keyval = List.combine keys values in
  insert mongo_theory keyval

let insert_dependency name nameDep =
  let md,id = name in
  let mdDep, idDep = nameDep in
  let values = List.map of_string [md; id; mdDep; idDep] in
  let keys = ["md";"id";"mdDep";"idDep"] in
  let keyval = List.combine keys values in
  insert mongo_dependencies keyval

let insert_item name kind =
  let md,id = name in
  let kind = Ast.string_of_kind kind in
  let values = List.map of_string [md; id; kind] in
  let keys = ["md"; "id"; "kind"] in
  let keyval = List.combine keys values in
  insert mongo_items keyval

let insert_printing name sys content =
  let md,id = name in
  let values = List.map of_string [md; id; sys; content] in
  let keys = ["md"; "id"; "sys"; "content"] in
  let keyval = List.combine keys values in
  insert mongo_printing keyval
