cnf(add_z, axiom, add(z, Y) = Y) .
cnf(add_s, axiom, add(s(X), Y) = s(add(X, Y))) .

cnf(hoge, negated_conjecture, add(s(add(z, z)), z) != s(z) ) .
