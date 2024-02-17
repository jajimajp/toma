%------------------------------------------------------------------------------
% File     : LCL292-10 : TPTP v7.5.0. Released v7.3.0.
% Domain   : Puzzles
% Problem  : Principia Mathematica 4.56
% Version  : Especial.
% English  :

% Refs     : [CS18]  Claessen & Smallbone (2018), Efficient Encodings of Fi
%          : [Sma18] Smallbone (2018), Email to Geoff Sutcliffe
% Source   : [Sma18]
% Names    :

% Status   : Satisfiable
% Rating   : 0.25 v7.5.0, 0.00 v7.3.0
% Syntax   : Number of clauses     :   12 (   0 non-Horn;  12 unit;   1 RR)
%            Number of atoms       :   12 (  12 equality)
%            Maximal clause size   :    1 (   1 average)
%            Number of predicates  :    1 (   0 propositional; 2-2 arity)
%            Number of functors    :   11 (   3 constant; 0-4 arity)
%            Number of variables   :   23 (   2 singleton)
%            Maximal term depth    :    5 (   3 average)
% SPC      : CNF_SAT_RFO_PEQ_UEQ

% Comments : Converted from LCL292-3 to UEQ using [CS18].
%------------------------------------------------------------------------------
cnf(ifeq_axiom,axiom,
    ( ifeq(A,A,B,C) = B )).

cnf(axiom_1_2,axiom,
    ( axiom(implies(or(A,A),A)) = true )).

cnf(axiom_1_3,axiom,
    ( axiom(implies(A,or(B,A))) = true )).

cnf(axiom_1_4,axiom,
    ( axiom(implies(or(A,B),or(B,A))) = true )).

cnf(axiom_1_5,axiom,
    ( axiom(implies(or(A,or(B,C)),or(B,or(A,C)))) = true )).

cnf(axiom_1_6,axiom,
    ( axiom(implies(implies(A,B),implies(or(C,A),or(C,B)))) = true )).

cnf(implies_definition,axiom,
    ( implies(X,Y) = or(not(X),Y) )).

cnf(rule_1,axiom,
    ( ifeq(axiom(X),true,theorem(X),true) = true )).

cnf(rule_2,axiom,
    ( ifeq(theorem(implies(Y,X)),true,ifeq(theorem(Y),true,theorem(X),true),true) = true )).

cnf(and_defn,axiom,
    ( and(P,Q) = not(or(not(P),not(Q))) )).

cnf(equivalent_defn,axiom,
    ( equivalent(P,Q) = and(implies(P,Q),implies(Q,P)) )).

cnf(prove_this,negated_conjecture,
    ( theorem(equivalent(and(not(p),not(q)),or(p,q))) != true )).

%------------------------------------------------------------------------------