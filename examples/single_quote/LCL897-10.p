%--------------------------------------------------------------------------
% File     : LCL897-10 : TPTP v8.1.2. Released v8.1.0.
% Domain   : Logic Calculi (Continuous Propositional)
% Problem  : Weak conjunction is associative in a hoop
% Version  : [AO13] axioms : Especial.
% English  : 

% Refs     : [Art12] Arthan (2012), Email to Geoff Sutcliffe
%          : [AO13]  Arthan & Olica (2013), Dual Hoops Have Unique Halving
%          : [Sma21] Smallbone (2021), Email to Geoff Sutcliffe
% Source   : [Sma21]
% Names    : 

% Status   : Unsatisfiable
% Rating   : 0.54 v8.1.0
% Syntax   : Number of clauses     :    9 (   9 unt;   0 nHn;   1 RR)
%            Number of literals    :    9 (   9 equ;   1 neg)
%            Maximal clause size   :    1 (   1 avg)
%            Maximal term depth    :    5 (   2 avg)
%            Number of predicates  :    1 (   0 usr;   0 prp; 2-2 aty)
%            Number of functors    :    6 (   6 usr;   4 con; 0-2 aty)
%            Number of variables   :   14 (   1 sgn)
% SPC      : CNF_UNS_RFO_PEQ_UEQ

% Comments : UEQ version, converted from LCL897+1.p
%--------------------------------------------------------------------------
cnf(sos_01,axiom,
    '+'('+'(A,B),C) = '+'(A,'+'(B,C)) ).

cnf(sos_02,axiom,
    '+'(A,B) = '+'(B,A) ).

cnf(sos_03,axiom,
    '+'(A,'0') = A ).

cnf(sos_04,axiom,
    ' = =>'(A,A) = '0' ).

cnf(sos_05,axiom,
    ' = =>'(A,'0') = '0' ).

cnf(sos_06,axiom,
    ' = =>'('0',A) = A ).

cnf(sos_07,axiom,
    ' = =>'('+'(A,B),C) = ' = =>'(A,' = =>'(B,C)) ).

cnf(sos_08,axiom,
    '+'(A,' = =>'(A,B)) = '+'(B,' = =>'(B,A)) ).

cnf(goals_09,negated_conjecture,
    '+'('+'(a,' = =>'(a,b)),' = =>'('+'(a,' = =>'(a,b)),c)) != '+'(a,' = =>'(a,'+'(b,' = =>'(b,c)))) ).

%------------------------------------------------------------------------------
