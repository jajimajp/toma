include('rev_list.ax').
% rev([1,2]) = [2,1]
cnf(hoge, negated_conjecture, ( rev(cons(z, cons(s(z), nil))) != cons(s(z), cons(z, nil)) ) ).
