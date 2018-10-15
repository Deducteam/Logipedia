type dagnode =
  {
    mutable children : Ast.NameSet.t
  }

type t =
  {
    nodes : (Ast.name, dagnode) Hashtbl.t
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
    Ast.NameSet.exists (fun a' -> is_acc a' b dag) anode.children

let is_acc a b dag =
  try
    if Hashtbl.mem dag.nodes b then
      is_acc a b dag
    else
      false
  with Not_found -> false

let add_node a dag =  Hashtbl.add dag.nodes a { children = Ast.NameSet.empty }

let add_edge a b dag =
  let anode = try Some (Hashtbl.find dag.nodes a) with Not_found -> None in
  let bnode = try Some (Hashtbl.find dag.nodes b) with Not_found -> None in
  match (anode, bnode) with
  | None, None ->
    Hashtbl.add dag.nodes a { children = Ast.NameSet.singleton b };
    Hashtbl.add dag.nodes b { children = Ast.NameSet.empty }
  | None, Some _ ->
    Hashtbl.add dag.nodes a { children = Ast.NameSet.singleton b }
  | Some aNode, None ->
    aNode.children <- Ast.NameSet.add b aNode.children;
    Hashtbl.add dag.nodes b { children = Ast.NameSet.empty }
  | Some aNode, Some bNode ->
    if not (is_acc a b dag) then
      aNode.children <- Ast.NameSet.add b aNode.children

let cpt = ref 0

let get dag a = (Hashtbl.find dag.nodes a)

let copy src =
  let nodes = Hashtbl.fold (fun k v acc -> (k,v) :: acc) src.nodes [] in
  let dst = init () in
  List.iter (fun (k,v) -> Hashtbl.add dst.nodes k {children = v.children}) nodes;
  dst

let reduce_node dag node =
  let children = (get dag node).children in
  let accessibles = ref Ast.NameSet.empty in
  Ast.NameSet.iter (fun child ->
      Ast.NameSet.iter (fun child' ->
          if child <> child' && is_acc child' child dag then
            accessibles := Ast.NameSet.add child !accessibles;
        ) children
    ) children;
  (get dag node).children <- Ast.NameSet.diff children !accessibles

let reduce dag =
  let reducedDag = copy dag in
  let nodes = Hashtbl.fold (fun k v l -> (k,v)::l) dag.nodes [] in
  List.iter (fun (x,xn) ->
      List.iter (fun (y,yn) ->
          List.iter (fun (z,zn) ->
              if is_acc x y dag && is_acc y z dag then
                if Ast.NameSet.mem z xn.children then (
                  Format.eprintf "%d@." !cpt;
                  incr cpt;
                  let xn = get reducedDag x in
                  xn.children <- Ast.NameSet.remove z xn.children
                );
            ) nodes
        ) nodes
    ) nodes;
  reducedDag
