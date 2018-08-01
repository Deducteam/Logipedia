def leibniz.leibniz : forall (A : Type) , (A) -> (A) -> Prop := fun (A : Type) , fun (x : A) , fun (y : A) , forall (P:(A) -> Prop) , ((P) (x)) -> (P) (y).
theorem leibniz.refl_leibniz : forall (A : Type) , forall (x:A) , (((leibniz.leibniz) (A)) (x)) (x) := fun (A : Type) , fun (x : A) , fun (P : (A) -> Prop) , fun (H : (P) (x)) , (H).
axiom leibniz.sym_leibniz : forall (A : Type) , forall (x:A) , forall (y:A) , ((((leibniz.leibniz) (A)) (x)) (y)) -> (((leibniz.leibniz) (A)) (y)) (x).
