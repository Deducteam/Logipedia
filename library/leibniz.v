Definition leibniz : forall (A:Type), (A) -> (A) -> Prop := fun (A:Type) => fun (x:A) => fun (y:A) => forall (P:(A) -> Prop), ((P) (x)) -> (P) (y).
Definition refl_leibniz : forall A, forall (x:A), (((leibniz) (A)) (x)) (x) := fun (A:Type) => fun (x:A) => fun (P:(A) -> Prop) => fun (H:(P) (x)) => (H).
Axiom sym_leibniz : forall A, forall (x:A), forall (y:A), ((((leibniz) (A)) (x)) (y)) -> (((leibniz) (A)) (y)) (x).
