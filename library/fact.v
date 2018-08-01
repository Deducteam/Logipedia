Require Import connectives.
Require Import leibniz.
Require Import nat.
Parameter fact : ((nat.nat) ) -> (nat.nat) .
Parameter fact_body : ((nat.nat) ) -> (nat.nat) .
Axiom axiom_fact : forall (n:(nat.nat) ), (((connectives.equal) ((nat.nat) )) (((fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact_body) )) (n)).
Definition eq_fact : forall (n:(nat.nat) ), (((leibniz.leibniz) ((nat.nat) )) (((fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact_body) )) (n)) := fun (n:(nat.nat) ) => ((((connectives.equal_leibniz) ((nat.nat) )) (((fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact_body) )) (n))) ((axiom_fact) (n)).
Definition sym_eq_fact : forall (n:(nat.nat) ), (((leibniz.leibniz) ((nat.nat) )) ((((nat.filter_nat_type) ((nat.nat) )) ((fact_body) )) (n))) (((fact) ) (n)) := fun (n:(nat.nat) ) => ((((leibniz.sym_leibniz) ((nat.nat) )) (((fact) ) (n))) ((((nat.filter_nat_type) ((nat.nat) )) ((fact_body) )) (n))) ((eq_fact) (n)).
Axiom axiom_fact_body_O : (((connectives.equal) ((nat.nat) )) (((fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) )).
Definition eq_fact_body_O : (((leibniz.leibniz) ((nat.nat) )) (((fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) )) := ((((connectives.equal_leibniz) ((nat.nat) )) (((fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) ))) (axiom_fact_body_O).
Definition sym_eq_fact_body_O : (((leibniz.leibniz) ((nat.nat) )) (((nat.S) ) ((nat.O) ))) (((fact_body) ) ((nat.O) )) := ((((leibniz.sym_leibniz) ((nat.nat) )) (((fact_body) ) ((nat.O) ))) (((nat.S) ) ((nat.O) ))) (eq_fact_body_O).
Axiom axiom_fact_body_S : forall (n:(nat.nat) ), (((connectives.equal) ((nat.nat) )) (((fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact) ) (n))) (((nat.S) ) (n))).
Definition eq_fact_body_S : forall (n:(nat.nat) ), (((leibniz.leibniz) ((nat.nat) )) (((fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact) ) (n))) (((nat.S) ) (n))) := fun (n:(nat.nat) ) => ((((connectives.equal_leibniz) ((nat.nat) )) (((fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact) ) (n))) (((nat.S) ) (n)))) ((axiom_fact_body_S) (n)).
Definition sym_eq_fact_body_S : forall (n:(nat.nat) ), (((leibniz.leibniz) ((nat.nat) )) ((((nat.times) ) (((fact) ) (n))) (((nat.S) ) (n)))) (((fact_body) ) (((nat.S) ) (n))) := fun (n:(nat.nat) ) => ((((leibniz.sym_leibniz) ((nat.nat) )) (((fact_body) ) (((nat.S) ) (n)))) ((((nat.times) ) (((fact) ) (n))) (((nat.S) ) (n)))) ((eq_fact_body_S) (n)).
