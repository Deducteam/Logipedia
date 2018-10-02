open Ast
open Sttforall
open Environ
open Term
open Basic
open Format

module CType  = Compile_type
module CTerm  = Compile_term
module CProof = Compile_proof

let sys = "sttfa"

let to_string fmt = Format.asprintf "%a" fmt

let mddep = Hashtbl.create 11

let iddep = Hashtbl.create 101

let update_id md id md' id' =
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
  | Arrow(left,right) -> Format.fprintf fmt "%a → %a" print__ty left print__ty right
  | TyOp(tyOp,list) -> Format.fprintf fmt "%s.%s" (fst tyOp) (snd tyOp)
  | Prop -> Format.fprintf fmt "ℙ"

let rec print_ty fmt = function
  | Ty _ty -> Format.fprintf fmt "%a" print__ty _ty
  | ForallK(var,ty) -> Format.eprintf "∀ %s, %a" var print_ty ty

let rec print__te fmt = function
  | TeVar x -> Format.fprintf fmt "%s" x
  | Abs(var,_, _te) -> Format.fprintf fmt "λ%s. %a" var print__te _te (*No need to print type *)
  | App(f,a) -> Format.fprintf fmt "%a %a" print__te f print__te a
  | Forall(var, _, _te) -> Format.eprintf "∀ %s, %a" var print__te _te
  | Impl(l,r) -> Format.fprintf fmt "%a ⇒ %a" print__te l print__te r
  | AbsTy(var, _te) -> Format.eprintf "%a" print__te _te
  | Cst(cst,_) -> Format.fprintf fmt "%s.%s" (fst cst) (snd cst)

let rec print_te fmt = function
  | Te _te -> Format.fprintf fmt "%a" print__te _te
  | ForallP(var,te) -> Format.eprintf "%a" print_te te

let compile_declaration name ty =
  let md,id = of_name name in
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
    Format.eprintf "%a@." print_ty (Compile.CType.compile_type Environ.empty_env a);
      Mongodb.insert_constant sys "" md id (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
    Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ;
    Format.eprintf "%a@." print__ty (Compile.CType.compile__type Environ.empty_env a);
      Mongodb.insert_constant sys "" md id (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ;
    Format.eprintf "%a@." print_te (Compile.CTerm.compile_term Environ.empty_env a);
      Mongodb.insert_axiom sys "" md id (to_string Pp.print_term a);
      matcher_dep (of_name name) a
  | Term.Const (_, _) when is_sttfa_const sttfa_type ty -> (* Partial function *)
    Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ;
    Mongodb.insert_constant sys "" md id (to_string Pp.print_term ty);
  | _ -> assert false

let compile_definition name ty term =
  let md,id = of_name name in
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
    Format.eprintf "[COMPILE] definition: %a@." Pp.print_name name ;
    Format.eprintf "%a@." print_ty (Compile.CType.compile_type Environ.empty_env a);
    Format.eprintf "%a@." print_te (Compile.CTerm.compile_term Environ.empty_env term);
      Mongodb.insert_definition sys "def" md id (to_string Pp.print_term a) (to_string Pp.print_term term);
      matcher_dep (of_name name) a;
      matcher_dep (of_name name) term
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
    Format.eprintf "[COMPILE] theorem: %a@." Pp.print_name name ;
    Format.eprintf "%a@." print_te (Compile.CTerm.compile_term Environ.empty_env a);
    Mongodb.insert_theorem sys "def" md id (to_string Pp.print_term a) (to_string Pp.print_term term);
    matcher_dep (of_name name) a;
    matcher_dep (of_name name) term
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
  Format.eprintf "%d@." (List.length entries);
  List.iter (handle_entry_dep md) entries;
  let md = string_of_mident md in
  let mds = QSet.of_list (Hashtbl.find mddep md) in
  let mds' = remove_transitive_deps mds in
  QSet.iter (fun md' -> Mongodb.insert_mdDep md md' "false") mds';
  let mds'' = QSet.diff mds mds' in
  QSet.iter (fun md' -> Mongodb.insert_mdDep md md' "true") mds''

let print_ast _ _ _ = ()
let print_bdd _ = ()
