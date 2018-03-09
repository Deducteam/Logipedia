type ty_var = string

type te_var = string

type name   = string * string

type tyOp   = name

type cst = name

type _ty = TyVar of ty_var
         | Arrow of _ty * _ty
         | TyOp of tyOp * _ty list
         | Prop

type ty  = ForallK of ty_var * ty
         | Ty of _ty



type _te = TeVar of te_var
         | Abs of te_var * _ty * _te
         | App of _te * _te
         | Forall of te_var * _ty * _te
         | Impl of _te * _te
         | AbsTy of ty_var * _te
         | Cst of cst * _ty list

type te  = ForallP of ty_var * te
         | Te of _te

type ty_ctx = ty_var list

type te_ctx =
  { ty:ty_var list;
    var: (te_var * _ty) list;
  }

type proof_ctx =
  {
    ctx:te_ctx;
    hyp: te list
  }

type judgment =
  {
    hyp:proof_ctx;
    thm:te;
  }

type proof =
  | Assume of judgment
  | ImplE of judgment * proof * proof
  | ImplI of judgment * proof
  | ForallE of judgment * proof * te
  | ForallI of judgment * proof
  | ForallTE of judgment * proof * ty
  | ForallTI of judgment * proof

type arity = int

type item =
    Parameter  of name * ty
  | Definition of name * ty * te
  | Axiom      of name * te
  | Theorem    of name * te * proof
  | TyOpDef    of tyOp * arity

type ast = item list
