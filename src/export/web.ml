open Basic
open Term
open Ast
open Sttforall
open Environ
open Format

(* FIXME: some hackish sutff for PVS, see other readme *)

let pp_name fmt (md,id) =
  Format.fprintf fmt "%s.%s" md id

let pp_item fmt item = Format.fprintf fmt "%a" pp_name (name_of_item item)

module Pretty =
struct
  let rec print__ty fmt = function
    | TyVar x -> Format.fprintf fmt "%s" x
    | Arrow(left,right) -> Format.fprintf fmt "%a → %a" print__ty_wp left print__ty right
    | TyOp(tyOp,[]) -> Format.fprintf fmt "%s.%s" (fst tyOp) (snd tyOp)
    | TyOp _ -> assert false (* Not handle right now, but eventually it should be *)
    | Prop -> Format.fprintf fmt "ℙ"
  and print__ty_wp fmt _ty =
    match _ty with
    | TyVar _
    | Prop
    | TyOp _ -> print__ty fmt _ty
    | Arrow _ -> Format.fprintf fmt "(%a)" print__ty _ty
  let rec print_ty fmt = function
    | Ty _ty -> Format.fprintf fmt "%a" print__ty _ty
    | ForallK(var,ty) -> Format.fprintf fmt "∀ %s, %a" var print_ty ty
  let is_operator (_te:Ast._te) = (* Only binary operator right now *)
    match _te with
    | App(App(Cst _,_),_) -> true
    | _ -> false

  let uop = Hashtbl.create 11

  let bop = Hashtbl.create 11

  let top = Hashtbl.create 11

  let _ =
    Hashtbl.add uop ("connectives","Not") (fun x -> "¬"^x);
    Hashtbl.add uop ("fact","fact") (fun x -> "!"^x);
    Hashtbl.add uop ("nat","pred") (fun x -> x^"-1");
    Hashtbl.add uop ("nat","S") (fun x -> x^"+1");
    Hashtbl.add bop ("connectives","Or") (fun x y -> x^" ∨ "^y);
    Hashtbl.add bop ("connectives","And") (fun x y -> x^" ∧ "^y);
    Hashtbl.add bop ("logic","eq") (fun x y -> x^" = "^y);
    Hashtbl.add bop ("nat","plus") (fun x y -> x^" + "^y);
    Hashtbl.add bop ("nat","minus") (fun x y -> x^" - "^y);
    Hashtbl.add bop ("nat","times") (fun x y -> x^" × "^y);
    Hashtbl.add bop ("nat","lt") (fun x y -> x^" < "^y);
    Hashtbl.add bop ("nat","le") (fun x y -> x^" ≤ "^y);
    Hashtbl.add bop ("nat","eqb") (fun x y -> x^" = "^y);
    Hashtbl.add bop ("primes","divides") (fun x y -> x^" | "^y);
    Hashtbl.add bop ("exp","exp") (fun x y -> x^" ^ "^y);
    Hashtbl.add top ("cong","congruent") (fun x y z -> x^" ≡ "^y^" ["^z^"]");
    Hashtbl.add top ("bool","match_bool_type") (fun x y z -> "if "^z^" then "^x^" else "^y)

  let symbol_of_unary_operator cst =
    Hashtbl.find uop cst

  let symbol_of_binary_operator cst =
    Hashtbl.find bop cst

  let symbol_of_ternary_operator cst =
    Hashtbl.find top cst

  let rec is_integer t =
    match t with
    | Cst(cst,_) -> (fst cst) = "nat" && (snd cst) = "O"
    | App(Cst(cst,_),l) ->
      (fst cst) = "nat" && (snd cst) = "S" && is_integer l
    | _ -> false

  let rec to_integer t =
    match t with
    | Cst _ -> 0
    | App(f,l) -> 1+(to_integer l)
    | _ -> assert false

  let rec print__te fmt = function
    | TeVar x -> Format.fprintf fmt "%s" x
    | Abs(var,_, _te) -> Format.fprintf fmt "λ%s. %a" var print__te _te (*No need to print type *)
    | App(f,a) -> print_app fmt f a
    | Forall(var, _, _te) -> print_forall fmt [var] _te
    | Impl(l,r) -> Format.fprintf fmt "%a ⇒ %a" print__te l print__te r
    | AbsTy(var, _te) -> Format.fprintf fmt "%a" print__te _te (* Types are not printed *)
    | Cst(cst,_) -> Format.fprintf fmt "%s" (snd cst)

  and print_forall fmt vars _te =
    match _te with
    | Forall(var',_,_te') ->
      print_forall fmt (var'::vars) _te'
    | _ ->
      let vars = (List.rev vars) in
      Format.fprintf fmt "∀ %a, %a" (Basic.pp_list " " Format.pp_print_string) vars print__te _te

  and print__te_wp fmt _te =
    match _te with
    | TeVar _
    | Cst _
    | AbsTy _ -> print__te fmt _te
    | App _ when is_integer _te -> print__te fmt _te
    | _ -> Format.fprintf fmt "(%a)" print__te _te

  and print_app fmt f r =
    let to_integer t = string_of_int (to_integer t) in
    match f with
    | Cst(cst,_) ->
      if is_integer (App(f,r)) then
        Format.fprintf fmt "%s@." (to_integer (App(f,r)))
      else
        begin
          try
            let r' = Format.asprintf "%a" print__te_wp r in
            Format.fprintf fmt "%s" (symbol_of_unary_operator cst r');
          with Not_found ->
            Format.fprintf fmt "%s %a" (snd cst) print__te_wp r
        end
    | App(Cst(cst,_),l) ->
      begin
        try
          let l' = Format.asprintf "%a" print__te_wp l in
          let r' = Format.asprintf "%a" print__te_wp r in
          Format.fprintf fmt "%s" (symbol_of_binary_operator cst l' r')
        with _ ->
          Format.fprintf fmt "%s %a %a" (snd cst) print__te_wp l print__te_wp r
      end
    | App(App(Cst(cst,_),l),m) ->
      begin
        try
          let l' = Format.asprintf "%a" print__te_wp l in
          let m' = Format.asprintf "%a" print__te_wp m in
          let r' = Format.asprintf "%a" print__te_wp r in
          Format.fprintf fmt "%s" (symbol_of_ternary_operator cst l' m' r')
        with _ ->
          Format.fprintf fmt "%s %a %a %a" (snd cst) print__te_wp l print__te_wp m print__te_wp r
      end
    | _ ->
      Format.fprintf fmt "%a %a" print__te f print__te_wp r

  let te_of_hyp j proof =
    let j' = judgment_of proof in
    let hyp = TeSet.diff j'.hyp j.hyp in
    assert (TeSet.cardinal hyp = 1);
    snd @@ (TeSet.choose hyp)

  let rec print_te fmt = function
    | Te _te -> Format.fprintf fmt "%a" print__te _te
    | ForallP(var,te) -> Format.fprintf fmt "%a" print_te te


  let pretty_print_item = function
    | Parameter(name, ty) ->
      Format.asprintf "%a" print_ty ty
    | TyOpDef(name,arity) ->
      Format.asprintf "%d" arity
    | Definition(name,ty,te) ->
      Format.asprintf "%a" print_te te
    | Theorem(name,te,_) ->
      Format.asprintf "%a" print_te te
    | Axiom(name,te) ->
      Format.asprintf "%a" print_te te

end

type web_item =
  {
    entry : Entry.entry;
    item  : item
  }

let items : (name, item) Hashtbl.t = Hashtbl.create 101

let db_insert_item item =
  Hashtbl.add items (name_of_item item) item;
  match item with
  | Parameter((md,id),_) ->
    Mongodb.insert_item md id "constant"
  | Definition((md,id),_,_) ->
    Mongodb.insert_item md id "definition"
  | Axiom((md,id),_) ->
    Mongodb.insert_item md id "axiom"
  | Theorem((md,id),_,_) ->
    Mongodb.insert_item md id "theorem"
  | TyOpDef((md,id),_) ->
    Mongodb.insert_item md id "tyop"

let db_insert_sys_item sys item =
  let (module E:Export.E) = Export.of_system sys in
  let md,id = name_of_item item in
  Mongodb.insert_printing md id (Systems.string_of_system E.system) (E.pretty_print_item item)

let db_insert_pretty_item item =
  let md,id = name_of_item item in
  Mongodb.insert_printing md id "dedukti" (Pretty.pretty_print_item item)

let db_insert_dep is_theory name name' =
  let insert = if is_theory then Mongodb.insert_theory else Mongodb.insert_dependency in
  let md,id = name in
  let md',id' = name' in
  insert md id md' id'

let is_axiom name =
  match Hashtbl.find items name with
  | Axiom _ -> true
  | _ -> false

let is_constant name =
  match Hashtbl.find items name with
  | Parameter _
  | TyOpDef _ -> true
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

(* FIXME: should be in pvs but this is not modular so... *)
let fix_pvs_deps ast name md =
  let table = Hashtbl.find env.item_deps name in
  let ast_of_md md = Hashtbl.find table md in
  let rec clot_refl md =
    if md <> "sttfa" then
      QSet.fold
        (fun md set -> QSet.union (clot_refl md) set) (ast_of_md md).dep (QSet.singleton md)
    else
      QSet.empty
  in
  let clot md =
    if md <> "sttfa" then
      QSet.fold
        (fun md set -> QSet.union (clot_refl md) set) (ast_of_md md).dep QSet.empty
    else
      QSet.empty
  in
  let trans_deps = QSet.fold (fun md set -> QSet.union (clot md) set) ast.dep QSet.empty in
  let pvs_deps = QSet.diff ast.dep trans_deps in
  {ast with dep = pvs_deps}

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
    | TyOpDef(name,_) -> NameSet.add name theory
    | Theorem(name,_,_) -> theory
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

let item_compare item item' = name_compare (name_of_item item) (name_of_item item')

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
  let md = fst @@ (name_of_item item) in
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
  | ForallK(var,ty) -> mk_ty_dep ty
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
  | ForallP(var,te) -> mk_te_dep te
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
  | Parameter(name,ty) -> mk_ty_dep ty
  | Definition(name,ty,te) -> mk_ty_dep ty; mk_te_dep te;
  | Axiom(name,te) -> mk_te_dep te;
  | Theorem(name,te,proof) -> mk_te_dep te; mk_proof_dep proof
  | TyOpDef(name,arity)  -> ()

let handle_web_item item =
  Format.eprintf "[WEB] %a@." pp_item item;
  let open Entry in
  db_insert_item item;
  let name = name_of_item item in
  let md = fst name in
  cur_name := name;
  add_ordered_name name;
  Hashtbl.add env.theory name (NameSet.empty);
  let ast_init = Hashtbl.create 3 in
  Hashtbl.add ast_init md {md; dep=QSet.empty;items=[item]};
  Hashtbl.add env.item_deps name ast_init;
  mk_item_dep item;
  mk_env_consistent (name_of_item item);
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
    let ast =
      match sys with
      | `Pvs ->
        let name = name_of_item item in
        let md = fst name in
        fix_pvs_deps ast name md
      | _ -> ast
    in
    E.print_ast fmt ast;
    close_out oc
  in
  db_insert_sys_item sys item;
  db_insert_pretty_item item;
  List.iter gen_sys_ast deps;
  let archive_name = Systems.string_of_system sys^"."^(string_of_name (name_of_item item))^".zip" in
  begin (* FIXME: should be generic. Probably pvs output can be changed to avoid that hack *)
    match sys with
    | `Pvs ->
      let cmd = Format.sprintf "cp library/pvs-strategies %s" tmp_dir in
      ignore(Sys.command cmd);
    | _ -> ()
  end;
  let pvs_strategies = if sys = `Pvs then "pvs-strategies" else "" in (*FIXME *)
  let cmd = Format.sprintf "cd %s && zip %s %s *.%s 2>&1 >/dev/null" tmp_dir archive_name pvs_strategies E.extension in
  if Sys.command cmd <> 0 then
    failwith "Error while trying to generate a zip file";
  close tmp_dir E.extension

let gen_file item =
  let deps = Hashtbl.find env.item_deps (name_of_item item) in
  let ldeps = Hashtbl.fold (fun _ v l -> v::l) deps [] in
  let ldeps = List.sort ast_compare ldeps in
  List.iter (gen_sys_archive item ldeps) [`Pvs]

let install_files () =
  ignore(Sys.command (Format.sprintf "mv /tmp/logipedia/*.zip website/web/theorems/download/files"))

let export_entries ast =
  Pvs.web := true; (* FIXME: fix computation of dependencies in PVS *)
  Hashtbl.add env.md_deps ast.md ast.dep;
  add_ordered_md ast.md;
  List.iter handle_web_item ast.items;
  List.iter gen_file ast.items;
  install_files ()
