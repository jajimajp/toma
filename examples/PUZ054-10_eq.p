cnf(two_whites_out_one_black_in,axiom,
    ( p(s(s(X)),Y) = p(X,s(Y))) ).

cnf(two_blacks_out_one_black_in,axiom,
    ( p(X,s(s(Y))) = p(X,s(Y))) ).

cnf(two_different_balls_out_one_white_in,axiom,
    ( p(s(X),s(Y)) = p(s(X),Y)) ).

cnf(goal, negated_conjecture,
(p(s(s(s(s(s(s(s(s(s(s(n0)))))))))),s(s(s(s(s(s(s(s(s(n0)))))))))) != p(s(n0),n0))).
