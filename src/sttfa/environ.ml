open Ast
open Kernel.Basic
module Basic = Kernel.Basic
module Term = Kernel.Term

let package = ref ""

let set_package file =
  let path = Filename.dirname file in
  let sep = Filename.dir_sep in
  assert (String.length sep = 1);
  let sep = String.get sep 0  in
  match List.rev @@ String.split_on_char sep path with
  | [] -> failwith "Files should be in a directory (which is interpreted as a package)"
  | x::_ -> package := x

let renaming = ref true

(* Use a list rather than a sec for List.mem_assoc *)
type proof_ctx = (string * _te) list

(* k counts lambas, used for renaming *)
type env =
  {k: int; dk: Term.typed_context; ty: ty_ctx; te: te_ctx; prf: proof_ctx}

let empty_env = {k= 0; dk= []; ty= []; te= []; prf= []}

let soi = string_of_ident

let rec gen_fresh_rec ctx avoid x c =
  let x' = if c < 0 then x else x ^ string_of_int c in
  if List.exists (fun (_, v, _) -> soi v = x') ctx || List.mem x' avoid
  then gen_fresh_rec ctx avoid x (c + 1)
  else mk_ident x'

let gen_fresh env avoid x = gen_fresh_rec env.dk avoid (soi x) (-1)

let mk_ident = mk_ident

let string_of_ident = string_of_ident

let of_name name = (string_of_mident (md name), string_of_ident (id name))

let name_of cst  = Basic.mk_name (Basic.mk_mident (fst cst)) (Basic.mk_ident (snd cst))

let add_ty_var env var =
  let open Basic in
  let open Sttfadk in
  { env with
    k= env.k + 1
  ; ty= var :: env.ty
  ; dk=
      (dloc, mk_ident var, Term.mk_Const dloc (mk_name sttfa_module sttfa_type))
      :: env.dk }

let add_ty_var_dk env var =
  add_ty_var env (soi var)

let add_te_var env var ty' =
  let open Basic in
  let ty = Decompile.decompile__type env.dk ty' in
  let ty = Decompile.to__type ty in
  { env with
    k = env.k + 1;
    te= (var, ty') :: env.te; dk= (dloc, mk_ident var, ty) :: env.dk
  }

let add_te_var_dk env var ty' = add_te_var env (soi var) ty'

let add_prf_ctx env id _te _te' =
  { env with
    k= env.k + 1
  ; prf= (id, _te') :: env.prf
  ; dk= (Basic.dloc, mk_ident id, _te) :: env.dk }

let get_dk_var env n =
  let _, x, _ = List.nth env.dk n in
  soi x

let rec take i l =
  if i = 0 then []
  else match l with [] -> assert false | x :: l -> x :: take (i - 1) l


let rec drop i l =
  if i = 0 then l
  else match l with [] -> assert false | _ :: l -> drop (i - 1) l

module StringxType = struct
  type t = string * _ty
  let compare = Stdlib.compare
end
(*
module TVars = Set.Make(StringxType)
*)
module Vars = Set.Make(String)

let gen_fresh_set varlist v =
  let rec gen_fresh_set_rec varlist v i =
    let new_v = v^(string_of_int i) in
    if List.mem new_v varlist then
      gen_fresh_set_rec varlist v (i+1)
    else new_v
  in
  gen_fresh_set_rec varlist v 0

let variables t =
  let rec variables_rec set_var = function
      TeVar(s) -> Vars.add s set_var
    | Abs(v,_,t) -> variables_rec (Vars.add v set_var) t
    | App(t1,t2) -> Vars.union (variables_rec set_var t1) (variables_rec set_var t2)
    | Forall(v,_,t) -> variables_rec (Vars.add v set_var) t
    | Impl(t1,t2) -> Vars.union (variables_rec set_var t1) (variables_rec set_var t2)
    | AbsTy(_,t) -> variables_rec set_var t
    | Cst _ -> set_var
  in
  variables_rec Vars.empty t
(*
let benign_spec = function
  | TeVar(v1),Forall(v2,_,_) when v1=v2-> true
  | _ -> false*)

let frees t =
  let rec frees_rec set_var = function
      TeVar(s) -> Vars.add s set_var
    | Abs(v,_,t) ->
      let set_vars_t = frees_rec set_var t in
      Vars.union set_var (Vars.remove v set_vars_t)
    | App(t1,t2) ->
      Vars.union (frees_rec set_var t1) (frees_rec set_var t2)
    | Forall(v,_,t) ->
      let set_vars_t = frees_rec set_var t in
      Vars.union set_var (Vars.remove v set_vars_t)
    | Impl(t1,t2) -> Vars.union (frees_rec set_var t1) (frees_rec set_var t2)
    | AbsTy(_,t) -> frees_rec set_var t
    | Cst _ -> set_var
  in
  frees_rec Vars.empty t

let frees_ty ty =
  let rec frees_ty_rec set_var = function
      TyVar(s) -> Vars.add s set_var
    | Arrow(tyl,tyr) ->
      Vars.union (frees_ty_rec set_var tyl) (frees_ty_rec set_var tyr)
    | TyOp(_,tys) ->
      let list_var_tys = List.map (frees_ty_rec set_var) tys in
      let set_vars_tys = List.fold_left (fun s1 -> fun s2 -> Vars.union s1 s2) Vars.empty list_var_tys in
      set_vars_tys
    | Prop -> Vars.empty
  in
  frees_ty_rec Vars.empty ty



(*let deep_alpha varlist t set_var =
  let rename_list = List.map (fun v -> (v,gen_fresh_set set_var v)) varlist in
  let rename v = List.assoc v rename_list in
  let rec deep_alpha_rec = function
      TeVar(v) when List.mem v varlist -> TeVar(rename v)
    | Abs(v,ty,t) when List.mem v varlist -> Abs(rename v,ty,deep_alpha_rec t)
    | Abs(v,ty,t) -> Abs(v,ty,deep_alpha_rec t)
    | App(t1,t2) -> App(deep_alpha_rec t1,deep_alpha_rec t2)
    | Forall(v,ty,t) when List.mem v varlist -> Forall(rename v,ty,deep_alpha_rec t)
    | Forall(v,ty,t) when List.mem v varlist -> Forall(rename v,ty,deep_alpha_rec t)
    | Impl(t1,t2) -> Impl(deep_alpha_rec t1,deep_alpha_rec t2)
    | AbsTy(_,t) -> t
    | t -> t in
  deep_alpha_rec t

let resolve_spec_conflict t1 t2 =
  if benign_spec(t1,t2) then t2
  else
    let frees_t1 = Vars.elements (frees t1) in
    let variables_t2 = variables t2 in
    deep_alpha frees_t1 t2 (Vars.elements variables_t2)
*)