cnf(s_definition,axiom,
    ( a(a(a(s,X),Y),Z) = a(a(X,Z),a(Y,Z)) )).

cnf(k_definition,axiom,
    ( a(a(k,X),Y) = X )).

cnf(prove_i_combinator,negated_conjecture,
    a(a(a(s, k),k), x) != x ).
