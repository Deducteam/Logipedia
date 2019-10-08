module A = Ast
module B = Kernel.Basic
module E = Parsers.Entry
module R = Kernel.Rule
module T = Kernel.Term

let add_dep : B.mident -> A.QSet.t =
 fun md -> A.QSet.singleton (B.string_of_mident @@ md)


let qset_of_list f l =
  List.fold_left (fun set x -> A.QSet.union (f x) set) A.QSet.empty l


(** Term / pattern / entry traversal commands. *)

let rec mk_term t =
  match t with
  | T.Kind | Type _ | DB _ -> A.QSet.empty
  | Const (_, c) -> add_dep (B.md c)
  | App (f, a, args) -> qset_of_list mk_term (f :: a :: args)
  | Lam (_, _, None, te) -> mk_term te
  | Lam (_, _, Some ty, te) -> A.QSet.union (mk_term ty) (mk_term te)
  | Pi (_, _, a, b) -> A.QSet.union (mk_term a) (mk_term b)


let rec mk_pattern p =
  match p with
  | R.Var (_, _, _, args) -> qset_of_list mk_pattern args
  | Pattern (_, c, args) ->
      A.QSet.union (add_dep (B.md c)) (qset_of_list mk_pattern args)
  | Lambda (_, _, te) -> mk_pattern te
  | Brackets t -> mk_term t


let mk_rule r = A.QSet.union (mk_pattern r.R.pat) (mk_term r.rhs)

let dep_of_entry = function
  | E.Decl (_, _, _, te) -> mk_term te
  | Def (_, _, _, None, te) -> mk_term te
  | Def (_, _, _, Some ty, te) -> A.QSet.union (mk_term ty) (mk_term te)
  | Rules (_, rs) -> qset_of_list mk_rule rs
  | Eval (_, _, te) -> mk_term te
  | Infer (_, _, te) -> mk_term te
  | Check (_, _, _, Convert (t1, t2)) -> A.QSet.union (mk_term t1) (mk_term t2)
  | Check (_, _, _, HasType (te, ty)) -> A.QSet.union (mk_term te) (mk_term ty)
  | DTree (_, _, _) -> A.QSet.empty
  | Print (_, _) -> A.QSet.empty
  | Name (_, _) -> A.QSet.empty
  | Require (_, md) -> add_dep md

let dep_of_entry (mds:B.mident list) e =
  List.fold_left (fun qset md -> A.QSet.remove (B.string_of_mident md) qset) (dep_of_entry e) mds
