import .connectives
import .leibniz
import .nat
constant fact.fact : ((nat.nat) ) -> (nat.nat) .
constant fact.fact_body : ((nat.nat) ) -> (nat.nat) .
axiom fact.axiom_fact : forall (n:(nat.nat) ) , (((connectives.equal) ((nat.nat) )) (((fact.fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact.fact_body) )) (n)).
theorem fact.eq_fact : forall (n:(nat.nat) ) , (((leibniz.leibniz) ((nat.nat) )) (((fact.fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact.fact_body) )) (n)) := fun (n : (nat.nat) ) , ((((@connectives.equal_leibniz) ((nat.nat) )) (((fact.fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact.fact_body) )) (n))) ((@fact.axiom_fact) (n)).
theorem fact.sym_eq_fact : forall (n:(nat.nat) ) , (((leibniz.leibniz) ((nat.nat) )) ((((nat.filter_nat_type) ((nat.nat) )) ((fact.fact_body) )) (n))) (((fact.fact) ) (n)) := fun (n : (nat.nat) ) , ((((@leibniz.sym_leibniz) ((nat.nat) )) (((fact.fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact.fact_body) )) (n))) ((@fact.eq_fact) (n)).
axiom fact.axiom_fact_body_O : (((connectives.equal) ((nat.nat) )) (((fact.fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) )).
theorem fact.eq_fact_body_O : (((leibniz.leibniz) ((nat.nat) )) (((fact.fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) )) := ((((@connectives.equal_leibniz) ((nat.nat) )) (((fact.fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) ))) (@fact.axiom_fact_body_O).
theorem fact.sym_eq_fact_body_O : (((leibniz.leibniz) ((nat.nat) )) (((nat.S) ) ((nat.O) ))) (((fact.fact_body) ) ((nat.O) )) := ((((@leibniz.sym_leibniz) ((nat.nat) )) (((fact.fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) ))) (@fact.eq_fact_body_O).
axiom fact.axiom_fact_body_S : forall (n:(nat.nat) ) , (((connectives.equal) ((nat.nat) )) (((fact.fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact.fact) ) (n))) (((nat.S) ) (n))).
theorem fact.eq_fact_body_S : forall (n:(nat.nat) ) , (((leibniz.leibniz) ((nat.nat) )) (((fact.fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact.fact) ) (n))) (((nat.S) ) (n))) := fun (n : (nat.nat) ) , ((((@connectives.equal_leibniz) ((nat.nat) )) (((fact.fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact.fact) ) (n))) (((nat.S) ) (n)))) ((@fact.axiom_fact_body_S) (n)).
theorem fact.sym_eq_fact_body_S : forall (n:(nat.nat) ) , (((leibniz.leibniz) ((nat.nat) )) ((((nat.times) ) (((fact.fact) ) (n))) (((nat.S) ) (n)))) (((fact.fact_body) ) (((nat.S) ) (n))) := fun (n : (nat.nat) ) , ((((@leibniz.sym_leibniz) ((nat.nat) )) (((fact.fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact.fact) ) (n))) (((nat.S) ) (n)))) ((@fact.eq_fact_body_S) (n)).
