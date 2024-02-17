cnf(add_z, axiom, add(z, Y) = Y) .
cnf(add_s, axiom, add(s(X), Y) = s(add(X, Y))) .
cnf(sp, axiom, s(p(X)) = X) .
cnf(ps, axiom, p(s(X)) = X) .

cnf(hoge, negated_conjecture, add(p(X), Y) != p(add(X, Y)) ) .
