include "basics/pts.ma".
axiom True : Prop.
axiom False : Prop.
axiom Not : (Prop) -> Prop.
axiom And : (Prop) -> (Prop) -> Prop.
axiom Or : (Prop) -> (Prop) -> Prop.
axiom ex : \forall A : Type[0] . ((A) -> Prop) -> Prop.
axiom equal : \forall A : Type[0] . (A) -> (A) -> Prop.
axiom I : (True) .
axiom falsity : \forall (t:Prop). ((False) ) -> t.
axiom nmk : \forall (A:Prop). ((A) -> (False) ) -> ((Not) ) (A).
axiom Not_ind : \forall (A:Prop). \forall (Q:Prop). (((A) -> (False) ) -> Q) -> (((Not) ) (A)) -> Q.
axiom conj : \forall (A:Prop). \forall (B:Prop). (A) -> (B) -> (((And) ) (A)) (B).
axiom match_And_prop : \forall (A:Prop). \forall (B:Prop). \forall (return_:Prop). ((A) -> (B) -> return_) -> ((((And) ) (A)) (B)) -> return_.
axiom or_introl : \forall (A:Prop). \forall (B:Prop). (A) -> (((Or) ) (A)) (B).
axiom or_intror : \forall (A:Prop). \forall (B:Prop). (B) -> (((Or) ) (A)) (B).
axiom match_Or_prop : \forall (A:Prop). \forall (B:Prop). \forall (return_:Prop). ((A) -> return_) -> ((B) -> return_) -> ((((Or) ) (A)) (B)) -> return_.
axiom ex_intro : \forall A. \forall (P:(A) -> Prop). \forall (x:A). ((P) (x)) -> ((ex) (A)) (P).
axiom match_ex_prop : \forall A. \forall (P:(A) -> Prop). \forall (return_:Prop). (\forall (x:A). ((P) (x)) -> return_) -> (((ex) (A)) (P)) -> return_.
axiom refl_equal : \forall A. \forall (x:A). (((equal) (A)) (x)) (x).
axiom equal_leibniz : \forall A. \forall (x:A). \forall (y:A). ((((equal) (A)) (x)) (y)) -> \forall (P:(A) -> Prop). ((P) (x)) -> (P) (y).
