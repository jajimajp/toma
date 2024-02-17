cnf(s_definition,axiom,
    ( a(a(a(s,X),Y),Z) = a(a(X,Z),a(Y,Z)) )).

cnf(k_definition,axiom,
    ( a(a(k,X),Y) = X )).

cnf(i_definition,axiom,
    ( a(i,X) = X )).

%----This is the U equivalent
cnf(prove_u_combinator,negated_conjecture,
    (  a(a(a(a(s,a(k,a(s,i))),a(a(s,i),i)),x),y) != a(y,a(a(x,x),y)) )).
