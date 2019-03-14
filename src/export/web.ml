open Ast

let pp_name fmt (md,id) = Format.fprintf fmt "%s.%s" md id

let pp_item fmt item = Format.fprintf fmt "%a" pp_name (name_of item)

module Pp = Web_pp

type web_item =
  {
    item     : Ast.item;
    theory   : NameSet.t;
    maindeps : NameSet.t;
    deps     : NameSet.t;
    package  : (string, web_item list) Hashtbl.t
  }

let db_insert_web_item wi =
  Mongodb.insert_item (name_of wi.item) (kind_of wi.item);
  let systems = Systems.systems in
  let insert_sys sys =
    let (module E:Export.E) = Export.of_system sys in
    let name = name_of wi.item in
    let str_sys = Systems.string_of_system E.system in
    let pp_item = E.pretty_print_item wi.item in
    Mongodb.insert_printing name str_sys pp_item;
  in
  List.iter insert_sys systems;
  Mongodb.insert_printing (name_of wi.item) "dedukti" (Pp.pretty_print_item wi.item);
  let insert_dep insert dep = insert (name_of wi.item) dep in
  NameSet.iter (insert_dep Mongodb.insert_theory) wi.theory;
  NameSet.iter (insert_dep Mongodb.insert_dependency) wi.maindeps


let items : (name, item) Hashtbl.t = Hashtbl.create 101

let db_insert_item item =
  Hashtbl.add items (name_of item) item;
  Mongodb.insert_item (name_of item) (kind_of item)

let db_insert_sys_item _ _ =  assert false
  (* Mongodb.insert_printing md id (Systems.string_of_system E.system) (E.pretty_print_item item) *)

let db_insert_pretty_item _ = assert false
  (*
  Mongodb.insert_printing md id "dedukti" (Pp.pretty_print_item item) *)

let db_insert_dep _ _ _ = assert false (*
  let insert = if is_theory then Mongodb.insert_theory else Mongodb.insert_dependency in
  let md,id = name in
  let md',id' = name' in
  insert md id md' id' *)

let is_axiom name =
  match Hashtbl.find items name with
  | Axiom _ -> true
  | _ -> false

let is_constant name =
  match Hashtbl.find items name with
  | Parameter _
  | TypeDecl _ -> true
  | _ -> false

let is_theorem name =
  match Hashtbl.find items name with
  | Theorem _ -> true
  | _ -> false

let is_definition name =
  match Hashtbl.find items name with
  | Definition _ -> true
  | _ -> false

type env = {
    item_deps     : (name, (string,ast) Hashtbl.t)  Hashtbl.t;
    theory        : (name, NameSet.t) Hashtbl.t;
    main_deps     : (name, NameSet.t) Hashtbl.t;
    undirect_deps : (name, NameSet.t) Hashtbl.t;
    md_deps       : (string, QSet.t) Hashtbl.t
  }

let env =
  {
    item_deps     = Hashtbl.create 101;
    theory        = Hashtbl.create 101;
    main_deps     = Hashtbl.create 101;
    undirect_deps = Hashtbl.create 101;
    md_deps       = Hashtbl.create 101;
  }

let get_deps name =
  let table = Hashtbl.find env.item_deps name in
  Hashtbl.fold (fun k v l -> (k,v.dep)::l) table []

let ordered_md = Hashtbl.create 11

let add_ordered_md : string -> unit =
  let counter = ref (-1) in
  fun md ->
    incr counter;
    Hashtbl.add ordered_md md !counter

let ordered_name = Hashtbl.create 101

let add_ordered_name : name -> unit =
  let counter = ref (-1) in
  fun name ->
    incr counter;
    Hashtbl.add ordered_name name !counter

let find table key =
  if Hashtbl.mem table key then
    Hashtbl.find table key
  else
    NameSet.empty

let update check add table key value =
  let deps = find table key in
  if not (check value deps) then
    Hashtbl.replace table key (add value deps)

let update_main_deps key value =
  update (NameSet.mem) (NameSet.add) env.main_deps key value

let update_unidrect_deps key dep =
  let ukey = find env.undirect_deps key in
  let udep = find env.undirect_deps dep in
  let udep = NameSet.union udep (find env.main_deps dep) in
  Hashtbl.replace env.undirect_deps key (NameSet.union ukey udep)

let update_theory key dep =
  let item_dep = Hashtbl.find items dep in
  let theory = NameSet.union (find env.theory key)  (find env.theory dep) in
  let theory' =
    match item_dep with
    | Parameter(name,_)
    | Axiom(name,_)
    | TypeDecl(name,_) -> NameSet.add name theory
    | TypeDef _ -> failwith "type definition not handled"
    | Theorem(_,_,_) -> theory
    | Definition(name,_,_) ->
      let item = Hashtbl.find items key in
      match item with
      | Axiom(_,_) -> NameSet.add name theory
      | _ -> theory
  in
  Hashtbl.replace env.theory key theory'

let name_compare name name' =
  Pervasives.compare (Hashtbl.find ordered_name name) (Hashtbl.find ordered_name name')

let md_compare md md' =
  Pervasives.compare (Hashtbl.find ordered_md md) (Hashtbl.find ordered_md md')

let item_compare item item' = name_compare (name_of item) (name_of item')

let ast_compare ast ast' = md_compare ast.md ast'.md

let find_ast table md =
  if Hashtbl.mem table md then
    Hashtbl.find table md
  else
    {md; dep=QSet.empty; items=[]}

let merge_ast ast ast' =
  let dep = QSet.union ast.dep ast'.dep in
  let items = List.sort_uniq item_compare (ast.items@ast'.items) in
  {ast with dep;items}

let merge_asts asts asts' =
  Hashtbl.iter (fun md ast' ->
      let ast = find_ast asts md in
      Hashtbl.replace asts md (merge_ast ast ast')) asts'

let asts_of_item item =
  let md = fst @@ (name_of item) in
  let ast_init = Hashtbl.create 3 in
  Hashtbl.add ast_init md {md; dep=QSet.empty;items=[item]};
  ast_init

let update_ast key dep =
  let item = Hashtbl.find items dep in
  let asts = Hashtbl.find env.item_deps key in
  merge_asts asts (asts_of_item item);
  let asts_dep = Hashtbl.find env.item_deps dep in
  merge_asts asts asts_dep;
  if fst key <> fst dep then
    let ast = Hashtbl.find asts (fst key) in
    Hashtbl.replace asts (fst key) {ast with dep = QSet.add (fst dep) ast.dep}

(* Can do better if name name' has been already seen *)
let mk_dep name name' =
  update_main_deps name name';
  update_unidrect_deps name name';
  update_theory name name';
  update_ast name name'

let mk_env_consistent name =
  let main_deps = find env.main_deps name in
  let undir_deps = find env.undirect_deps name in
  let main_deps' = NameSet.diff main_deps undir_deps in
  Hashtbl.replace env.main_deps name main_deps'

let cur_name = ref ("","")

let rec mk__ty_dep = function
  | TyVar _ | Prop -> ()
  | TyOp(name',_tys) -> mk_dep !cur_name name'; List.iter mk__ty_dep _tys
  | Arrow(_tyl,_tyr) -> mk__ty_dep _tyl; mk__ty_dep _tyr

let rec mk_ty_dep = function
  | ForallK(_,ty) -> mk_ty_dep ty
  | Ty(_ty) -> mk__ty_dep _ty

let rec mk__te_dep = function
  | TeVar _ -> ()
  | Abs(_,_ty,_te) -> mk__ty_dep _ty; mk__te_dep _te
  | App(_tel,_ter) -> mk__te_dep _tel; mk__te_dep _ter
  | Forall(_,_ty,_te) -> mk__ty_dep _ty; mk__te_dep _te
  | Impl(_tel,_ter) -> mk__te_dep _tel; mk__te_dep _ter
  | AbsTy(_,_te) -> mk__te_dep _te
  | Cst(name',_tys) -> mk_dep !cur_name name'; List.iter mk__ty_dep _tys

let rec mk_te_dep = function
  | ForallP(_,te) -> mk_te_dep te
  | Te(_te) ->         mk__te_dep _te

let rec mk_proof_dep = function
  | Assume _ -> ()
  | Lemma(name',_) ->        mk_dep !cur_name name'
  | Conv(_,proof,_) ->       mk_proof_dep proof
  | ImplE(_,prfl,prfr) ->    mk_proof_dep prfl; mk_proof_dep prfr
  | ImplI(_,proof,_) ->      mk_proof_dep proof
  | ForallE(_,proof,_te) ->  mk__te_dep _te; mk_proof_dep proof
  | ForallI(_,proof,_) ->    mk_proof_dep proof
  | ForallPE(_,proof,_ty) -> mk__ty_dep _ty; mk_proof_dep proof
  | ForallPI(_,proof,_) ->   mk_proof_dep proof

let mk_item_dep = function
  | Parameter(_,ty) -> mk_ty_dep ty
  | Definition(_,ty,te) -> mk_ty_dep ty; mk_te_dep te;
  | Axiom(_,te) -> mk_te_dep te;
  | Theorem(_,te,proof) -> mk_te_dep te; mk_proof_dep proof
  | TypeDecl(_,_)  -> ()
  | TypeDef(_,_,ty) -> mk__ty_dep ty

let handle_web_item item =
  Format.eprintf "[WEB] %a@." pp_item item;
  db_insert_item item;
  let name = name_of item in
  let md = fst name in
  cur_name := name;
  add_ordered_name name;
  Hashtbl.add env.theory name (NameSet.empty);
  let ast_init = Hashtbl.create 3 in
  Hashtbl.add ast_init md {md; dep=QSet.empty;items=[item]};
  Hashtbl.add env.item_deps name ast_init;
  mk_item_dep item;
  mk_env_consistent (name_of item);
  NameSet.iter (fun name' ->
      db_insert_dep true name name') (Hashtbl.find env.theory name);
  NameSet.iter (fun name' ->
      db_insert_dep false name name') (Hashtbl.find env.main_deps name)

let init tmp_dir =
  try ignore(Sys.is_directory tmp_dir);
  with _ -> Unix.mkdir tmp_dir 0o766

let close tmp_dir ext =
  ignore(Sys.command (Format.sprintf "rm %s/*.%s" tmp_dir ext))

let gen_sys_archive item deps sys =
  Format.eprintf "[FILE %s] %a@." (Systems.string_of_system sys) pp_item item;
  let (module E:Export.E) = Export.of_system sys in
  let tmp_dir = Filename.get_temp_dir_name () ^ "/logipedia" in
  init tmp_dir;
  let gen_sys_ast ast =
    let path = tmp_dir^"/"^ast.md^"."^E.extension in
    let oc = open_out path in
    let fmt = Format.formatter_of_out_channel oc in
    assert (ast.items <> []);
    (* Compute real modules dependencies *)
    let mdeps = get_deps (name_of item) in
    E.print_ast ~mdeps fmt ast;
    close_out oc
  in
  (*
  db_insert_sys_item sys item;
  db_insert_pretty_item item; *)
  List.iter gen_sys_ast deps;
  let archive_name = Systems.string_of_system sys^"."^(string_of_name (name_of item))^".zip" in
  let cmd = Format.sprintf "cd %s && zip %s *.%s 2>&1 >/dev/null" tmp_dir archive_name E.extension in
  if Sys.command cmd <> 0 then
    failwith "Error while trying to generate a zip file";
  close tmp_dir E.extension

let gen_file item =
  let deps = Hashtbl.find env.item_deps (name_of item) in
  let ldeps = Hashtbl.fold (fun _ v l -> v::l) deps [] in
  let ldeps = List.sort ast_compare ldeps in
  List.iter (gen_sys_archive item ldeps) [`Pvs]

let install_files () =
  ignore(Sys.command (Format.sprintf "mv /tmp/logipedia/*.zip export/web"))

(*
let item_of_name : name -> item = failwith "todo"

let rec web_item_of : item -> web_item = fun item ->
  let theory   = theory_of item in
  let deps     = deps_of item in
  let maindeps = maindeps_of item in
  let package  = package_of item in
  {item;theory;deps;maindeps;package}

and theory_of : ?is_axiom:bool -> item -> NameSet.t = fun ?is_axiom item ->
  let deps = deps_of item in
  failwith "todo"

and deps_of       : item -> NameSet.t = fun _ -> failwith "todo deps_of"
and maindeps_of   : item -> NameSet.t = fun _ -> failwith "todo maindeps"
and package_of    : item -> (string,web_item list) Hashtbl.t = fun _ -> failwith "todo package" in failwith "todo"
*)

let export_entries : ast -> unit = fun ast ->
  Hashtbl.add env.md_deps ast.md ast.dep;
  add_ordered_md ast.md;
  List.iter handle_web_item ast.items;
  List.iter gen_file ast.items;
  install_files ()
