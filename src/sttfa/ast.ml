(** This file defines a full AST of STTforall.
    Informations are redundant to facilitate exportation. *)

module B = Kernel.Basic
module D = Core.Deps

(** {b NOTE} underscored types are monomorphic, not underscored are
    polymorphic. *)

type ty_var  = string

type te_var  = string

type hyp_var = string

type name = string * string

type cst = name

type _ty =
  | TyVar of ty_var
  | Arrow of _ty * _ty
  | TyOp of name * _ty list
  | Prop

let dummy__ty = TyVar("dummy")

type ty = ForallK of ty_var * ty | Ty of _ty

let dummy_ty = Ty(dummy__ty)

type _te =
  | TeVar of te_var
  | Abs of te_var * _ty * _te
  | App of _te * _te
  | Forall of te_var * _ty * _te
  | Impl of _te * _te
  | AbsTy of ty_var * _te
  | Cst of cst * _ty list

type te = ForallP of ty_var * te | Te of _te

type ty_ctx = ty_var list

type te_ctx = (te_var * _ty) list

module TeSet = Set.Make (struct
  type t = hyp_var * _te

  let compare = compare
end)

type hyp = TeSet.t

type judgment = {ty: ty_ctx; te: te_ctx; hyp: hyp; thm: te}

type ctx =
  | CAbs
  | CAppL
  | CAppR
  | CForall
  | CImplL
  | CImplR
  | CAbsTy
  | CForallP

let print_ctx fmt = function
  | CAbs     -> Format.fprintf fmt "CAbs"
  | CAppL    -> Format.fprintf fmt "CAppL"
  | CAppR    -> Format.fprintf fmt "CAppR"
  | CForall  -> Format.fprintf fmt "CForall"
  | CImplL   -> Format.fprintf fmt "CImplL"
  | CImplR   -> Format.fprintf fmt "CImplR"
  | CAbsTy   -> Format.fprintf fmt "CAbsTy"
  | CForallP -> Format.fprintf fmt "CForallP"

let print_ctxs fmt ctxs =
  B.pp_list "," print_ctx fmt ctxs

type redex = Delta of name * _ty list | Beta of _te

let print_redex oc r =
  match r with
  | Delta((md,id),_tys) -> Format.fprintf oc "%s,%s" md id
  | Beta _ -> Format.fprintf oc "beta"

type rewrite_seq = (redex * ctx list) list

type trace = {left: rewrite_seq; right: rewrite_seq}

let print_rewrite_ctx oc (rw,ctxs) =
  Format.fprintf oc "unfold %a at %a;@." print_redex rw print_ctxs ctxs

let print_rewrite_seq oc rws = List.iter (print_rewrite_ctx oc) rws

let print_trace oc trace =
  Format.fprintf oc "left:@.%a@." print_rewrite_seq trace.left;
  Format.fprintf oc "right:@.%a@." print_rewrite_seq trace.right

type proof =
  | Assume of judgment * hyp_var
  | Lemma of name * judgment
  | Conv of judgment * proof * trace
  | ImplE of judgment * proof * proof
  | ImplI of judgment * proof * hyp_var
  | ForallE of judgment * proof * _te
  | ForallI of judgment * proof * te_var
  | ForallPE of judgment * proof * _ty
  | ForallPI of judgment * proof * ty_var

type arity = int

type item =
  | Parameter of name * ty
  | Definition of name * ty * te
  | Axiom of name * te
  | Theorem of name * te * proof
  | TypeDecl of name * arity
  | TypeDef of name * ty_var list * _ty

type kind = [`Parameter | `Definition | `Axiom | `Theorem | `TypeDecl | `TypeDef ]

type ast = {
  md   : string;
  dep  : D.QSet.t;
  items: item list}

type mdeps = (string * D.QSet.t) list

let kind_of = function
  | Parameter _   -> `Parameter
  | Definition _  -> `Definition
  | Axiom _       -> `Axiom
  | Theorem _     -> `Theorem
  | TypeDecl _    -> `TypeDecl
  | TypeDef _     -> `TypeDef


let string_of_kind = function
  | `Parameter   -> "parameter"
  | `Definition  -> "definition"
  | `Axiom       -> "axiom"
  | `Theorem     -> "theorem"
  | `TypeDecl    -> "tyop"
  | `TypeDef     -> "type definition"

let name_of = function
  | Parameter(name,_)
  | Definition(name,_,_)
  | Axiom(name,_)
  | Theorem(name,_,_)
  | TypeDecl(name,_)
  | TypeDef(name,_,_) -> name

let judgment_of = function
  | Assume(j,_)     -> j
  | Lemma(_,j)      -> j
  | Conv(j,_,_)     -> j
  | ImplE(j,_,_)    -> j
  | ImplI(j,_,_)    -> j
  | ForallE(j,_,_)  -> j
  | ForallI(j,_,_)  -> j
  | ForallPE(j,_,_) -> j
  | ForallPI(j,_,_) -> j

let string_of_name (md,id) = Format.sprintf "%s.%s" md id
