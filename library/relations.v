Require Import connectives.
Require Import logic.
Definition reflexive : forall (A:Type), ((A) -> (A) -> Prop) -> Prop := fun (A:Type) => fun (R:(A) -> (A) -> Prop) => forall (x:A), ((R) (x)) (x).
Definition transitive : forall (A:Type), ((A) -> (A) -> Prop) -> Prop := fun (A:Type) => fun (R:(A) -> (A) -> Prop) => forall (x:A), forall (y:A), forall (z:A), (((R) (x)) (y)) -> (((R) (y)) (z)) -> ((R) (x)) (z).
Definition RC : forall (A:Type), ((A) -> (A) -> Prop) -> (A) -> (A) -> Prop := fun (A:Type) => fun (R:(A) -> (A) -> Prop) => fun (x:A) => fun (y:A) => (((connectives.Or) ) (((R) (x)) (y))) ((((logic.eq) (A)) (x)) (y)).
Definition RC_reflexive : forall A, forall (R:(A) -> (A) -> Prop), ((reflexive) (A)) (((RC) (A)) (R)) := fun (A:Type) => fun (R:(A) -> (A) -> Prop) => fun (x:A) => (((connectives.or_intror) (((R) (x)) (x))) ((((logic.eq) (A)) (x)) (x))) (((logic.refl) (A)) (x)).
Definition injective : forall (A:Type), forall (B:Type), ((A) -> B) -> Prop := fun (A:Type) => fun (B:Type) => fun (f:(A) -> B) => forall (x:A), forall (y:A), ((((logic.eq) (B)) ((f) (x))) ((f) (y))) -> (((logic.eq) (A)) (x)) (y).
Definition commutative : forall (A:Type), ((A) -> (A) -> A) -> Prop := fun (A:Type) => fun (f:(A) -> (A) -> A) => forall (x:A), forall (y:A), (((logic.eq) (A)) (((f) (x)) (y))) (((f) (y)) (x)).
Definition associative : forall (A:Type), ((A) -> (A) -> A) -> Prop := fun (A:Type) => fun (f:(A) -> (A) -> A) => forall (x:A), forall (y:A), forall (z:A), (((logic.eq) (A)) (((f) (((f) (x)) (y))) (z))) (((f) (x)) (((f) (y)) (z))).
Definition monotonic : forall (A:Type), ((A) -> (A) -> Prop) -> ((A) -> A) -> Prop := fun (A:Type) => fun (R:(A) -> (A) -> Prop) => fun (f:(A) -> A) => forall (x:A), forall (y:A), (((R) (x)) (y)) -> ((R) ((f) (x))) ((f) (y)).
Definition distributive : forall (A:Type), ((A) -> (A) -> A) -> ((A) -> (A) -> A) -> Prop := fun (A:Type) => fun (f:(A) -> (A) -> A) => fun (g:(A) -> (A) -> A) => forall (x:A), forall (y:A), forall (z:A), (((logic.eq) (A)) (((f) (x)) (((g) (y)) (z)))) (((g) (((f) (x)) (y))) (((f) (x)) (z))).
