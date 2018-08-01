Parameter True : Prop.
Parameter False : Prop.
Parameter Not : (Prop) -> Prop.
Parameter And : (Prop) -> (Prop) -> Prop.
Parameter Or : (Prop) -> (Prop) -> Prop.
Parameter ex : forall (A:Type), ((A) -> Prop) -> Prop.
Parameter equal : forall (A:Type), (A) -> (A) -> Prop.
Axiom I : (True) .
Axiom falsity : forall (t:Prop), ((False) ) -> t.
Axiom nmk : forall (A:Prop), ((A) -> (False) ) -> ((Not) ) (A).
Axiom Not_ind : forall (A:Prop), forall (Q:Prop), (((A) -> (False) ) -> Q) -> (((Not) ) (A)) -> Q.
Axiom conj : forall (A:Prop), forall (B:Prop), (A) -> (B) -> (((And) ) (A)) (B).
Axiom match_And_prop : forall (A:Prop), forall (B:Prop), forall (return_:Prop), ((A) -> (B) -> return_) -> ((((And) ) (A)) (B)) -> return_.
Axiom or_introl : forall (A:Prop), forall (B:Prop), (A) -> (((Or) ) (A)) (B).
Axiom or_intror : forall (A:Prop), forall (B:Prop), (B) -> (((Or) ) (A)) (B).
Axiom match_Or_prop : forall (A:Prop), forall (B:Prop), forall (return_:Prop), ((A) -> return_) -> ((B) -> return_) -> ((((Or) ) (A)) (B)) -> return_.
Axiom ex_intro : forall A, forall (P:(A) -> Prop), forall (x:A), ((P) (x)) -> ((ex) (A)) (P).
Axiom match_ex_prop : forall A, forall (P:(A) -> Prop), forall (return_:Prop), (forall (x:A), ((P) (x)) -> return_) -> (((ex) (A)) (P)) -> return_.
Axiom refl_equal : forall A, forall (x:A), (((equal) (A)) (x)) (x).
Axiom equal_leibniz : forall A, forall (x:A), forall (y:A), ((((equal) (A)) (x)) (y)) -> forall (P:(A) -> Prop), ((P) (x)) -> (P) (y).
