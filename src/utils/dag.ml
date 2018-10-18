type 'a dagnode =
  {
    mutable children : 'a list
  }

type 'a t =
  {
    nodes : ('a, 'a dagnode) Hashtbl.t
  }

let init () =
  {
    nodes = Hashtbl.create 11
  }

let length dag = Hashtbl.length dag.nodes

let rec is_acc a b dag =
  if a = b then
    true
  else
    let anode = Hashtbl.find dag.nodes a in
    List.exists (fun a' -> is_acc a' b dag) anode.children

let is_acc a b dag =
  try
    if Hashtbl.mem dag.nodes b then
      is_acc a b dag
    else
      false
  with Not_found -> false

let add_node a dag =  Hashtbl.add dag.nodes a { children = [] }

let add_edge a b dag =
  let anode = try Some (Hashtbl.find dag.nodes a) with Not_found -> None in
  let bnode = try Some (Hashtbl.find dag.nodes b) with Not_found -> None in
  match (anode, bnode) with
  | None, None ->
    Hashtbl.add dag.nodes a { children = [b] };
    Hashtbl.add dag.nodes b { children = [] }
  | None, Some _ ->
    Hashtbl.add dag.nodes a { children = [b] }
  | Some aNode, None ->
    aNode.children <- b::aNode.children;
    Hashtbl.add dag.nodes b { children = [] }
  | Some aNode, Some bNode ->
    if not (is_acc a b dag) then
      aNode.children <- b::aNode.children

let cpt = ref 0

let get dag a = Hashtbl.find dag.nodes a

let copy src =
  let nodes = Hashtbl.fold (fun k v acc -> (k,v) :: acc) src.nodes [] in
  let dst = init () in
  List.iter (fun (k,v) -> Hashtbl.add dst.nodes k {children = v.children}) nodes;
  dst
(*
let reduce_node dag node =
  let children = (get dag node).children in
  let accessibles = ref [] in
  List.iter (fun child ->
      List.iter (fun child' ->
          if child <> child' && is_acc child' child dag then
            accessibles := child::!accessibles;
        ) children
    ) children;
  (get dag node).children <- Ast.NameSet.diff children !accessibles
                             *)

let dfs dag a =
  let dependencies = ref [] in
  let add_name (md,id) =
    if List.mem_assoc md !dependencies then
      let md_dep = List.assoc md !dependencies in
      if not (List.mem id !md_dep) then
        md_dep := id::!md_dep;
    else
      dependencies := (md, ref [id])::!dependencies
  in
  let rec dfs seen dag a =
    let node = Hashtbl.find dag.nodes a in
    List.iter (fun name ->
        dfs (name::seen) dag name;
        add_name name
      ) node.children;
  in
  dfs [a] dag a;
  List.fold_left (fun res (md,l) -> (md,List.rev !l)::res) [] !dependencies
