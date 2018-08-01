include "basics/pts.ma".
include "connectives.ma".
include "leibniz.ma".
include "nat.ma".
axiom fact : ((nat) ) -> (nat) .
axiom fact_body : ((nat) ) -> (nat) .
axiom axiom_fact : \forall (n:(nat) ). (((equal) ((nat) )) (((fact) ) (n))) ((((filter_nat_type) ((nat) )) ((fact_body) )) (n)).
definition eq_fact : \forall (n:(nat) ). (((leibniz) ((nat) )) (((fact) ) (n))) ((((filter_nat_type) ((nat) )) ((fact_body) )) (n)) := \lambda n : (nat) . ((((equal_leibniz) ((nat) )) (((fact) ) (n))) ((((filter_nat_type) ((nat) )) ((fact_body) )) (n))) ((axiom_fact) (n)).
definition sym_eq_fact : \forall (n:(nat) ). (((leibniz) ((nat) )) ((((filter_nat_type) ((nat) )) ((fact_body) )) (n))) (((fact) ) (n)) := \lambda n : (nat) . ((((sym_leibniz) ((nat) )) (((fact) ) (n))) ((((filter_nat_type) ((nat) )) ((fact_body) )) (n))) ((eq_fact) (n)).
axiom axiom_fact_body_O : (((equal) ((nat) )) (((fact_body) ) ((O) ))) (((S) ) ((O) )).
definition eq_fact_body_O : (((leibniz) ((nat) )) (((fact_body) ) ((O) ))) (((S) ) ((O) )) := ((((equal_leibniz) ((nat) )) (((fact_body) ) ((O) ))) (((S) ) ((O) ))) (axiom_fact_body_O).
definition sym_eq_fact_body_O : (((leibniz) ((nat) )) (((S) ) ((O) ))) (((fact_body) ) ((O) )) := ((((sym_leibniz) ((nat) )) (((fact_body) ) ((O) ))) (((S) ) ((O) ))) (eq_fact_body_O).
axiom axiom_fact_body_S : \forall (n:(nat) ). (((equal) ((nat) )) (((fact_body) ) (((S) ) (n)))) ((((times) ) (((fact) ) (n))) (((S) ) (n))).
definition eq_fact_body_S : \forall (n:(nat) ). (((leibniz) ((nat) )) (((fact_body) ) (((S) ) (n)))) ((((times) ) (((fact) ) (n))) (((S) ) (n))) := \lambda n : (nat) . ((((equal_leibniz) ((nat) )) (((fact_body) ) (((S) ) (n)))) ((((times) ) (((fact) ) (n))) (((S) ) (n)))) ((axiom_fact_body_S) (n)).
definition sym_eq_fact_body_S : \forall (n:(nat) ). (((leibniz) ((nat) )) ((((times) ) (((fact) ) (n))) (((S) ) (n)))) (((fact_body) ) (((S) ) (n))) := \lambda n : (nat) . ((((sym_leibniz) ((nat) )) (((fact_body) ) (((S) ) (n)))) ((((times) ) (((fact) ) (n))) (((S) ) (n)))) ((eq_fact_body_S) (n)).
