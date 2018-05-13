open Ast
open Basic

let renaming = ref true

(* Use a list rather than a sec for List.mem_assoc *)
type proof_ctx = (string * _te) list

(* k counts lambas, used for renaming *)
type env =
  {k: int; dk: Term.typed_context; ty: ty_ctx; te: te_ctx; prf: proof_ctx}

let empty_env = {k= 0; dk= []; ty= []; te= []; prf= []}

let soi = string_of_ident

let rec gen_fresh ctx x c =
  let x' = if c < 0 then x else x ^ string_of_int c in
  if List.exists (fun (_, v, _) -> soi v = x') ctx then gen_fresh ctx x (c + 1)
  else mk_ident x'

let gen_fresh env x = gen_fresh env.dk (soi x) (-1)

let of_name name = (string_of_mident (md name), string_of_ident (id name))

let name_of cst  = Basic.mk_name (Basic.mk_mident (fst cst)) (Basic.mk_ident (snd cst))

let add_ty_var env var =
  let open Basic in
  let open Sttforall in
  { env with
    k= env.k + 1
  ; ty= var :: env.ty
  ; dk=
      (dloc, mk_ident var, Term.mk_Const dloc (mk_name sttfa_module sttfa_type))
      :: env.dk }

let add_te_var env var ty' =
  let open Basic in
  let ty = Decompile.decompile__type env.dk ty' in
  let ty = Decompile.to__type ty in
  { env with
    k = env.k + 1;
    te= (var, ty') :: env.te; dk= (dloc, mk_ident var, ty) :: env.dk
  }

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
