constant connectives.True : Prop.
constant connectives.False : Prop.
constant connectives.Not : (Prop) -> Prop.
constant connectives.And : (Prop) -> (Prop) -> Prop.
constant connectives.Or : (Prop) -> (Prop) -> Prop.
constant connectives.ex : forall (A : Type) , ((A) -> Prop) -> Prop.
constant connectives.equal : forall (A : Type) , (A) -> (A) -> Prop.
axiom connectives.I : (connectives.True) .
axiom connectives.falsity : forall (t:Prop) , ((connectives.False) ) -> t.
axiom connectives.nmk : forall (A:Prop) , ((A) -> (connectives.False) ) -> ((connectives.Not) ) (A).
axiom connectives.Not_ind : forall (A:Prop) , forall (Q:Prop) , (((A) -> (connectives.False) ) -> Q) -> (((connectives.Not) ) (A)) -> Q.
axiom connectives.conj : forall (A:Prop) , forall (B:Prop) , (A) -> (B) -> (((connectives.And) ) (A)) (B).
axiom connectives.match_And_prop : forall (A:Prop) , forall (B:Prop) , forall (return:Prop) , ((A) -> (B) -> return) -> ((((connectives.And) ) (A)) (B)) -> return.
axiom connectives.or_introl : forall (A:Prop) , forall (B:Prop) , (A) -> (((connectives.Or) ) (A)) (B).
axiom connectives.or_intror : forall (A:Prop) , forall (B:Prop) , (B) -> (((connectives.Or) ) (A)) (B).
axiom connectives.match_Or_prop : forall (A:Prop) , forall (B:Prop) , forall (return:Prop) , ((A) -> return) -> ((B) -> return) -> ((((connectives.Or) ) (A)) (B)) -> return.
axiom connectives.ex_intro : forall (A : Type) , forall (P:(A) -> Prop) , forall (x:A) , ((P) (x)) -> ((connectives.ex) (A)) (P).
axiom connectives.match_ex_prop : forall (A : Type) , forall (P:(A) -> Prop) , forall (return:Prop) , (forall (x:A) , ((P) (x)) -> return) -> (((connectives.ex) (A)) (P)) -> return.
axiom connectives.refl_equal : forall (A : Type) , forall (x:A) , (((connectives.equal) (A)) (x)) (x).
axiom connectives.equal_leibniz : forall (A : Type) , forall (x:A) , forall (y:A) , ((((connectives.equal) (A)) (x)) (y)) -> forall (P:(A) -> Prop) , ((P) (x)) -> (P) (y).
