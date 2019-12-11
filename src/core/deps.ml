open Kernel.Basic
open Kernel.Term
open Parsing.Entry
open Kernel.Rule

(** {1 Dedukti dependency computation} *)

module QSet = Set.Make(String)

let add_dep : mident -> QSet.t =
 fun md -> QSet.singleton (string_of_mident @@ md)

let qset_of_list f l =
  List.fold_left (fun set x -> QSet.union (f x) set) QSet.empty l

(** Term / pattern / entry traversal commands. *)

let rec mk_term t =
  match t with
  | Kind | Type _ | DB _ -> QSet.empty
  | Const (_, c) -> add_dep (md c)
  | App (f, a, args) -> qset_of_list mk_term (f :: a :: args)
  | Lam (_, _, None, te) -> mk_term te
  | Lam (_, _, Some ty, te) -> QSet.union (mk_term ty) (mk_term te)
  | Pi (_, _, a, b) -> QSet.union (mk_term a) (mk_term b)

let rec mk_pattern p =
  match p with
  | Var (_, _, _, args) -> qset_of_list mk_pattern args
  | Pattern (_, c, args) ->
      QSet.union (add_dep (md c)) (qset_of_list mk_pattern args)
  | Lambda (_, _, te) -> mk_pattern te
  | Brackets t -> mk_term t


let mk_rule r = QSet.union (mk_pattern r.pat) (mk_term r.rhs)

let dep_of_entry = function
  | Decl (_, _, _, te) -> mk_term te
  | Def (_, _, _, None, te) -> mk_term te
  | Def (_, _, _, Some ty, te) -> QSet.union (mk_term ty) (mk_term te)
  | Rules (_, rs) -> qset_of_list mk_rule rs
  | Eval (_, _, te) -> mk_term te
  | Infer (_, _, te) -> mk_term te
  | Check (_, _, _, Convert (t1, t2)) -> QSet.union (mk_term t1) (mk_term t2)
  | Check (_, _, _, HasType (te, ty)) -> QSet.union (mk_term te) (mk_term ty)
  | DTree (_, _, _) -> QSet.empty
  | Print (_, _) -> QSet.empty
  | Name (_, _) -> QSet.empty
  | Require (_, md) -> add_dep md

let dep_of_entry (mds:mident list) e =
  List.fold_left (fun qset md -> QSet.remove (string_of_mident md) qset)
    (dep_of_entry e) mds
