let db_name = "logipedia"

let mongo_theory = assert false
  (* Mongo.create_local_default db_name "theory" *)
let mongo_dependencies = assert false
  (* Mongo.create_local_default db_name "dependencies" *)
let mongo_items = assert false (* Mongo.create_local_default db_name "items" *)
let mongo_printing = assert false
  (* Mongo.create_local_default db_name "printing" *)

let of_string = assert false (* Bson.create_string *)

let insert collection keyval = ()
  (* let doc = List.fold_left (fun doc (key,value) -> Bson.add_element key value doc) Bson.empty keyval in *)
  (* Mongo.insert collection [doc] *)

let insert_theory md id mdDep idDep = ()
  (* let values = List.map of_string [md; id; mdDep; idDep] in *)
  (* let keys = ["md";"id";"mdDep";"idDep"] in *)
  (* let keyval = List.combine keys values in *)
  (* insert mongo_theory keyval *)

let insert_dependency md id mdDep idDep = ()
  (* let values = List.map of_string [md; id; mdDep; idDep] in *)
  (* let keys = ["md";"id";"mdDep";"idDep"] in *)
  (* let keyval = List.combine keys values in *)
  (* insert mongo_dependencies keyval *)

let insert_item md id kind = ()
  (* let values = List.map of_string [md; id; kind] in *)
  (* let keys = ["md"; "id"; "kind"] in *)
  (* let keyval = List.combine keys values in *)
  (* insert mongo_items keyval *)

let insert_printing md id sys content = ()
  (* let values = List.map of_string [md; id; sys; content] in *)
  (* let keys = ["md"; "id"; "sys"; "content"] in *)
  (* let keyval = List.combine keys values in *)
  (* insert mongo_printing keyval *)
