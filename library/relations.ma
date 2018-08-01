include "basics/pts.ma".
include "connectives.ma".
include "logic.ma".
definition reflexive : \forall A : Type[0] . ((A) -> (A) -> Prop) -> Prop := \lambda A : Type[0]. \lambda R : (A) -> (A) -> Prop. \forall (x:A). ((R) (x)) (x).
definition transitive : \forall A : Type[0] . ((A) -> (A) -> Prop) -> Prop := \lambda A : Type[0]. \lambda R : (A) -> (A) -> Prop. \forall (x:A). \forall (y:A). \forall (z:A). (((R) (x)) (y)) -> (((R) (y)) (z)) -> ((R) (x)) (z).
definition RC : \forall A : Type[0] . ((A) -> (A) -> Prop) -> (A) -> (A) -> Prop := \lambda A : Type[0]. \lambda R : (A) -> (A) -> Prop. \lambda x : A. \lambda y : A. (((Or) ) (((R) (x)) (y))) ((((eq) (A)) (x)) (y)).
definition RC_reflexive : \forall A. \forall (R:(A) -> (A) -> Prop). ((reflexive) (A)) (((RC) (A)) (R)) := \lambda A : Type[0]. \lambda R : (A) -> (A) -> Prop. \lambda x : A. (((or_intror) (((R) (x)) (x))) ((((eq) (A)) (x)) (x))) (((refl) (A)) (x)).
definition injective : \forall A : Type[0] . \forall B : Type[0] . ((A) -> B) -> Prop := \lambda A : Type[0]. \lambda B : Type[0]. \lambda f : (A) -> B. \forall (x:A). \forall (y:A). ((((eq) (B)) ((f) (x))) ((f) (y))) -> (((eq) (A)) (x)) (y).
definition commutative : \forall A : Type[0] . ((A) -> (A) -> A) -> Prop := \lambda A : Type[0]. \lambda f : (A) -> (A) -> A. \forall (x:A). \forall (y:A). (((eq) (A)) (((f) (x)) (y))) (((f) (y)) (x)).
definition associative : \forall A : Type[0] . ((A) -> (A) -> A) -> Prop := \lambda A : Type[0]. \lambda f : (A) -> (A) -> A. \forall (x:A). \forall (y:A). \forall (z:A). (((eq) (A)) (((f) (((f) (x)) (y))) (z))) (((f) (x)) (((f) (y)) (z))).
definition monotonic : \forall A : Type[0] . ((A) -> (A) -> Prop) -> ((A) -> A) -> Prop := \lambda A : Type[0]. \lambda R : (A) -> (A) -> Prop. \lambda f : (A) -> A. \forall (x:A). \forall (y:A). (((R) (x)) (y)) -> ((R) ((f) (x))) ((f) (y)).
definition distributive : \forall A : Type[0] . ((A) -> (A) -> A) -> ((A) -> (A) -> A) -> Prop := \lambda A : Type[0]. \lambda f : (A) -> (A) -> A. \lambda g : (A) -> (A) -> A. \forall (x:A). \forall (y:A). \forall (z:A). (((eq) (A)) (((f) (x)) (((g) (y)) (z)))) (((g) (((f) (x)) (y))) (((f) (x)) (z))).
