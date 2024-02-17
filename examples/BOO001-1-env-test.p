% $ TPTP=Axioms/ toma --casc BOO001-1-env-test.p 
include('BOO001-0.ax').
cnf(prove_inverse_is_self_cancelling,negated_conjecture,
    inverse(inverse(a)) != a ).
