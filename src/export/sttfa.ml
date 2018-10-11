open Term
open Ast
open Sttforall
open Environ
open Basic
open Format

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

module Closure =
struct

  let closure : (Ast.name, (string * (Ast.name list)) list) Hashtbl.t = Hashtbl.create 91

  let init name = Hashtbl.add closure name []

  (*  let shift n set = NameSet.map (fun (order,name) -> (order+n,name)) set *)

  let to_set l =
    List.fold_left
        (fun set (s,l) -> List.fold_left
            (fun set name -> NameSet.add name set) set l) NameSet.empty l

  let merge base newdeps =
    let rec add base name =
      match base with
      | [] -> [fst name, [name]]
      | (md,l)::base' -> if fst name = md then (md,l@[name])::base' else (md,l)::(add base' name)
    in
    List.fold_left (fun base name -> add base name) base newdeps

  let add_dep name nameDep =
    let sdep = NameSet.add nameDep (to_set (Hashtbl.find closure nameDep)) in
    let dep  = Hashtbl.find closure name in
    let sdiff = NameSet.diff sdep (to_set dep) in
    let dep' = merge dep (NameSet.elements sdiff) in
    Hashtbl.replace closure name dep'

  let close name : unit =
    let rec concat l =
      match l with
      | [] -> []
      | (_,x)::t -> x@(concat t)
    in
    Mongodb.insert_closureidDep (fst name) (snd name) (concat (Hashtbl.find closure name))
end

let sys = "sttfa"

let to_string fmt = Format.asprintf "%a" fmt

let mddep = Hashtbl.create 11

let iddep = Hashtbl.create 101

let update_id md id md' id' =
  if md' <> sys then
    Closure.add_dep (md,id) (md',id');
  let l = try Hashtbl.find iddep (md,id) with _ -> [] in
  let l' = (md',id')::l in
  Hashtbl.replace iddep (md,id) l'

let update_md md md' =
  let l = try Hashtbl.find mddep md with _ -> [] in
  let l' = md'::l in
  Hashtbl.replace mddep md l'

let new_iddep md id md' id' =
  not (Hashtbl.mem iddep (md,id)) ||
  not (List.mem (md',id') (Hashtbl.find iddep (md,id)))

let new_mddep md md' =
  md <> md' && (not (Hashtbl.mem mddep md) || not (List.mem md' (Hashtbl.find mddep md)))

let fct_insert_dependances (md,id) (md',id') =
  if new_iddep md id md' id' then
    begin
      if new_mddep md md' then
        update_md md md'; (* module dependencies are inserted at thend *)
      Mongodb.insert_idDep md id md' id';
      update_id md id md' id'
    end

let rec matcher_dep (md,id) a =
  match a with
  |Kind -> ()
  |Type(_) -> ()
  |DB(_, _, _) -> ()
  |Const(_, name') -> fct_insert_dependances (md,id) (of_name name')
  |App(ter, term, term_lst) -> matcher_dep (md,id) ter; matcher_dep (md,id) term; List.iter (matcher_dep (md,id)) term_lst
  |Lam(_, _, _, term) -> matcher_dep (md,id) term
  |Pi(_, _, ter, term) -> matcher_dep (md,id) ter; matcher_dep (md,id) term


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

type couple_spine =
  | SForall of string * _te
  | SImpl of _te * string

type proof_spine =
  | SLemma of (string * string) * te * couple_spine list
  | SHyp of string * te * couple_spine list

let rec unfold_spine_left proof =
  match proof with
  | ImplE(_,proof,_) ->    unfold_spine_left proof
  | ForallE(_,proof,_) ->  unfold_spine_left proof
  | ForallPE(_,proof,_) -> unfold_spine_left proof
  | Lemma(name,j) -> SLemma(name,j.thm, [])
  | Assume(j,h) -> SHyp(h,j.thm, [])
  | Conv(_,proof,_) -> unfold_spine_left proof
  | _ -> assert false

let rec print_proof beginning fmt proof =
  match proof with
  | Assume(j,h) -> Format.eprintf "use %s" h
  | Lemma(name,j) ->
    Format.eprintf "use %s" (snd name)
  | Conv(j,proof,trace) ->
    print_proof beginning fmt proof
  | ImplE (j,proofl, proofr) ->
    begin
      match unfold_spine_left proofl with
      | SLemma(name,te,_) ->
        Format.eprintf "Since %a from theorem {%s} then@." print_te j.thm (snd name)
      | SHyp(h,te,_) ->
        Format.eprintf "Since %a from variable {%s} then@." print_te te h
    end;
    Format.eprintf "ImplL@.";
    print_proof false fmt proofl;
    Format.eprintf "ImplR@.";
    print_proof false fmt proofr;
  | ImplI(j,proof,var) ->
    Format.eprintf "Assume %a as [%s]\n" print__te (te_of_hyp j proof) var;
    print_proof false fmt proof
  | ForallE(j,proof, _te) ->
    Format.eprintf "ForallE@.";
    print_proof false fmt proof
  | ForallI(j,proof,te_var) ->
    if beginning then
      print_proof true fmt proof
    else
      begin
        Format.eprintf "Let %s : TODO\n" te_var;
        print_proof true fmt proof
      end
  | ForallPE(j,proof, _ty) ->
    print_proof false fmt proof
  | ForallPI(j,proof,ty_var) ->
    print_proof true fmt proof

module SetString = Set.Make(struct type t = string * string let compare = compare end)

let rec get__dfns _te =
  match _te with
  | TeVar x -> SetString.empty
  | Abs(var,_, _te) -> get__dfns _te
  | App(f,a) -> SetString.union (get__dfns f) (get__dfns a)
  | Forall(var, _, _te) -> get__dfns _te
  | Impl(l,r) -> SetString.union (get__dfns l) (get__dfns r)
  | AbsTy(var, _te) -> get__dfns _te
  | Cst(cst,_) -> SetString.singleton cst

let rec get_dfns te =
  match te with
  | Te _te -> get__dfns _te
  | ForallP(_,te) -> get_dfns te

let print_prelude_proof fmt theorem proof =
  let defns = get_dfns theorem in
  if SetString.cardinal defns > 0 then
    Format.eprintf "Recall that:@.";
  SetString.iter (fun cst ->
      let name = (Environ.name_of cst) in
      if not (Env.is_static dloc (Environ.name_of cst)) then
        let cfg = Reduction.({default_cfg with select = Some (fun m -> m = Rule.Delta name)}) in
        let te = Env.reduction ~red:cfg (Term.mk_Const dloc name) in
        let te' = (CTerm.compile_term empty_env te) in
        Format.eprintf "- %s is defined as:@.%a@." (snd cst) print_te te') defns;
  Format.eprintf "@."

let print_proof theorem fmt proof =
  print_prelude_proof fmt theorem proof;
  print_proof true fmt proof


let rec has__lambda = function
  | Cst _
  | TeVar _ -> false
  | Abs _ -> true
  | App(l,r) -> has__lambda l || has__lambda r
  | Forall(_,_,t) -> has__lambda t
  | Impl(l,r) -> has__lambda l || has__lambda r
  | AbsTy(_,te) -> has__lambda te

let rec has_lambda = function
  | ForallP(_,te) -> has_lambda te
  | Te te -> has__lambda te

let rec is__ho = function
  | Cst _
  | TeVar _ -> false
  | Abs(_,_,t) -> is__ho t
  | App(l,r) -> is__ho l || is__ho r
  | Forall(_,Arrow _,t) -> true
  | Forall(_,_,t) -> is__ho t
  | Impl(l,r) -> is__ho l || is__ho r
  | AbsTy(_,te) -> is__ho te

let rec is_ho = function
  | ForallP(_,te) -> is_ho te
  | Te te -> is__ho te


module TySet = Set.Make(struct type t = _ty let compare = compare end)

let all_of_them = ref TySet.empty

let insert _ty =
  if TySet.mem _ty !all_of_them then
    ()
  else (
    Format.eprintf "check:type %a@." print__ty _ty;
    all_of_them := TySet.add _ty !all_of_them
  )

let rec check__term = function
  | Cst(_,tys) -> List.iter insert tys
  | TeVar _ -> ()
  | Abs(_,_,_te) -> check__term _te
  | App(l,r) -> check__term l; check__term r
  | Forall(_,_,t) -> check__term t
  | Impl(l,r) -> check__term l; check__term r
  | AbsTy(_,te) -> check__term te

let rec check_term = function
  | ForallP(_,te) -> check_term te
  | Te _te -> check__term _te

let rec check_proof = function
  | Assume _ -> ()
  | Lemma _ -> ()
  | Conv(_,p,_) -> check_proof p
  | ImplE(_,pl,pr) -> check_proof pl; check_proof pr
  | ImplI(_,p,_) -> check_proof p
  | ForallE(_,p,_) -> check_proof p
  | ForallI(_,p,_) -> check_proof p
  | ForallPE(_,p,_ty) -> insert _ty; check_proof p
  | ForallPI(_,p,_) -> check_proof p

let compile_declaration name ty =
  let md,id = of_name name in
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[BDD] constant: %a@." Pp.print_name name ;
    let a' = Format.asprintf "%a@." print_ty (Compile.CType.compile_type Environ.empty_env a) in
    Mongodb.insert_kind md id "constant";
    Mongodb.insert_constant sys "" md id a';
    Closure.init (of_name name);
    matcher_dep (of_name name) a;
    Closure.close (of_name name)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    Format.eprintf "[BDD] constant: %a@." Pp.print_name name ;
    let a' = Format.asprintf "%a@." print__ty (Compile.CType.compile__type Environ.empty_env a) in
    Mongodb.insert_kind md id "constant";
    Mongodb.insert_constant sys "" md id a';
    Closure.init (of_name name);
    matcher_dep (of_name name) a;
    Closure.close (of_name name)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[BDD] axiom: %a@." Pp.print_name name ;
    let a' = Format.asprintf "%a@." print_te (Compile.CTerm.compile_term Environ.empty_env a) in
    Mongodb.insert_kind md id "axiom";
    Mongodb.insert_axiom sys "" md id a';
    Closure.init (of_name name);
    matcher_dep (of_name name) a;
    Closure.close (of_name name);
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty -> (* Partial function *)
    Format.eprintf "[BDD] typeop: %a@." Pp.print_name name ;
    Closure.init (of_name name);
    Mongodb.insert_kind md id "typeop";
    Mongodb.insert_constant sys "" md id (to_string Pp.print_term ty);
    Closure.close (of_name name)
  | _ -> assert false

let compile_definition name ty term =
  let md,id = of_name name in
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[BDD] definition: %a@." Pp.print_name name ;
    let a' = Format.asprintf "%a@." print_ty (Compile.CType.compile_type Environ.empty_env a) in
    let term' = Format.asprintf "%a@." print_te (Compile.CTerm.compile_term Environ.empty_env term) in
    Mongodb.insert_kind md id "definition";
    Mongodb.insert_definition sys "def" md id a' term';
    Closure.init (of_name name);
    matcher_dep (of_name name) a;
    matcher_dep (of_name name) term;
    Closure.close (of_name name)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[BDD] theorem: %a@." Pp.print_name name ;
    let a' = Compile.CTerm.compile_term Environ.empty_env a in
    let str_a' = Format.asprintf "%a@." print_te a' in
    Mongodb.insert_kind md id "theorem";
    Mongodb.insert_theorem sys "def" md id str_a' (to_string Pp.print_term term);
    Closure.init (of_name name);
    matcher_dep (of_name name) a;
    matcher_dep (of_name name) term;
    Closure.close (of_name name)
  | _ -> assert false

let handle_entry_dep md e =
  let open Entry in
  match e with
  | Decl (lc, id, st, ty) ->
    ( Env.declare lc id st ty;
      compile_declaration (mk_name md id) ty )
  | Def (lc, id, opaque, Some ty, te) ->
    ( Env.define lc id opaque te (Some ty);
      compile_definition (mk_name md id) ty te )
  | Def   _ -> failwith "Definition without types are not supported"
  | Rules _ -> failwith "Rules are not part of the sttforall logic"
  | _       -> failwith "Commands are not supported"

let remove_transitive_deps deps =
  let remove_dep dep deps =
    let md = Basic.mk_mident dep in
    let md_deps = Signature.get_md_deps Basic.dloc md in
    QSet.diff deps (QSet.of_list (List.map Basic.string_of_mident md_deps))
  in
  QSet.fold remove_dep deps deps

let handle_dep md entries =
  List.iter (handle_entry_dep md) entries;
  let md = string_of_mident md in
  let mds = QSet.of_list (Hashtbl.find mddep md) in
  let mds' = remove_transitive_deps mds in
  QSet.iter (fun md' -> Mongodb.insert_mdDep md md' "false") mds';
  let mds'' = QSet.diff mds mds' in
  QSet.iter (fun md' -> Mongodb.insert_mdDep md md' "true") mds''

let print_ast _ _ _ = ()
let print_bdd _ = ()
