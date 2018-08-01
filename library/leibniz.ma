include "basics/pts.ma".
definition leibniz : \forall A : Type[0] . (A) -> (A) -> Prop := \lambda A : Type[0]. \lambda x : A. \lambda y : A. \forall (P:(A) -> Prop). ((P) (x)) -> (P) (y).
definition refl_leibniz : \forall A. \forall (x:A). (((leibniz) (A)) (x)) (x) := \lambda A : Type[0]. \lambda x : A. \lambda P : (A) -> Prop. \lambda H : (P) (x). (H).
axiom sym_leibniz : \forall A. \forall (x:A). \forall (y:A). ((((leibniz) (A)) (x)) (y)) -> (((leibniz) (A)) (y)) (x).
