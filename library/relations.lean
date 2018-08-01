import .connectives
import .logic
def relations.reflexive : forall (A : Type) , ((A) -> (A) -> Prop) -> Prop := fun (A : Type) , fun (R : (A) -> (A) -> Prop) , forall (x:A) , ((R) (x)) (x).
def relations.transitive : forall (A : Type) , ((A) -> (A) -> Prop) -> Prop := fun (A : Type) , fun (R : (A) -> (A) -> Prop) , forall (x:A) , forall (y:A) , forall (z:A) , (((R) (x)) (y)) -> (((R) (y)) (z)) -> ((R) (x)) (z).
def relations.RC : forall (A : Type) , ((A) -> (A) -> Prop) -> (A) -> (A) -> Prop := fun (A : Type) , fun (R : (A) -> (A) -> Prop) , fun (x : A) , fun (y : A) , (((connectives.Or) ) (((R) (x)) (y))) ((((logic.eq_) (A)) (x)) (y)).
theorem relations.RC_reflexive : forall (A : Type) , forall (R:(A) -> (A) -> Prop) , ((relations.reflexive) (A)) (((relations.RC) (A)) (R)) := fun (A : Type) , fun (R : (A) -> (A) -> Prop) , fun (x : A) , (((@connectives.or_intror) (((R) (x)) (x))) ((((logic.eq_) (A)) (x)) (x))) (((@logic.refl_) (A)) (x)).
def relations.injective : forall (A : Type) , forall (B : Type) , ((A) -> B) -> Prop := fun (A : Type) , fun (B : Type) , fun (f : (A) -> B) , forall (x:A) , forall (y:A) , ((((logic.eq_) (B)) ((f) (x))) ((f) (y))) -> (((logic.eq_) (A)) (x)) (y).
def relations.commutative : forall (A : Type) , ((A) -> (A) -> A) -> Prop := fun (A : Type) , fun (f : (A) -> (A) -> A) , forall (x:A) , forall (y:A) , (((logic.eq_) (A)) (((f) (x)) (y))) (((f) (y)) (x)).
def relations.associative : forall (A : Type) , ((A) -> (A) -> A) -> Prop := fun (A : Type) , fun (f : (A) -> (A) -> A) , forall (x:A) , forall (y:A) , forall (z:A) , (((logic.eq_) (A)) (((f) (((f) (x)) (y))) (z))) (((f) (x)) (((f) (y)) (z))).
def relations.monotonic : forall (A : Type) , ((A) -> (A) -> Prop) -> ((A) -> A) -> Prop := fun (A : Type) , fun (R : (A) -> (A) -> Prop) , fun (f : (A) -> A) , forall (x:A) , forall (y:A) , (((R) (x)) (y)) -> ((R) ((f) (x))) ((f) (y)).
def relations.distributive : forall (A : Type) , ((A) -> (A) -> A) -> ((A) -> (A) -> A) -> Prop := fun (A : Type) , fun (f : (A) -> (A) -> A) , fun (g : (A) -> (A) -> A) , forall (x:A) , forall (y:A) , forall (z:A) , (((logic.eq_) (A)) (((f) (x)) (((g) (y)) (z)))) (((g) (((f) (x)) (y))) (((f) (x)) (z))).
