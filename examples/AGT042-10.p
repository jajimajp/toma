%------------------------------------------------------------------------------
% File     : AGT042-10 : TPTP v7.5.0. Released v7.3.0.
% Domain   : Puzzles
% Problem  : Axioms for CPlanT
% Version  : Especial.
% English  : 

% Refs     : [CS18]  Claessen & Smallbone (2018), Efficient Encodings of Fi
%          : [Sma18] Smallbone (2018), Email to Geoff Sutcliffe
% Source   : [Sma18]
% Names    :

% Status   : Satisfiable
% Rating   : 0.00 v7.5.0, 0.25 v7.3.0
% Syntax   : Number of clauses     :  709 (   0 non-Horn; 709 unit; 617 RR)
%            Number of atoms       :  709 ( 709 equality)
%            Maximal clause size   :    1 (   1 average)
%            Number of predicates  :    1 (   0 propositional; 2-2 arity)
%            Number of functors    :   77 (  63 constant; 0-4 arity)
%            Number of variables   :  124 (  12 singleton)
%            Maximal term depth    :    7 (   2 average)
% SPC      : CNF_SAT_RFO_PEQ_UEQ

% Comments : Converted from AGT042+1 to UEQ using [CS18].
%------------------------------------------------------------------------------
cnf(ifeq_axiom,axiom,
    ( ifeq3(A,A,B,C) = B )).

cnf(ifeq_axiom_001,axiom,
    ( ifeq2(A,A,B,C) = B )).

cnf(ifeq_axiom_002,axiom,
    ( ifeq(A,A,B,C) = B )).

cnf(a1_1_3,axiom,
    ( ifeq3(accept_number(A,N),true,ifeq3(accept_leader(A,L),true,ifeq3(accept_city(A,C),true,accept_team(A,L,C,N),true),true),true) = true )).

cnf(a1_1_2,axiom,
    ( ifeq3(accept_team(A,L,C,N),true,accept_number(A,N),true) = true )).

cnf(a1_1_1,axiom,
    ( ifeq3(accept_team(A,L,C,N),true,accept_leader(A,L),true) = true )).

cnf(a1_1,axiom,
    ( ifeq3(accept_team(A,L,C,N),true,accept_city(A,C),true) = true )).

cnf(a1_2,axiom,
    ( ifeq3(less(M,N),true,ifeq3(accept_number(A,N),true,accept_number(A,M),true),true) = true )).

cnf(a1_3,axiom,
    ( ifeq3(accept_population(A,P,N),true,ifeq3(less(M,N),true,accept_population(A,P,M),true),true) = true )).

cnf(a1_4_1,axiom,
    ( ifeq3(the_agent_in_all_proposed_teams(A,L,C),true,accept_leader(A,L),true) = true )).

cnf(a1_4,axiom,
    ( ifeq3(the_agent_in_all_proposed_teams(A,L,C),true,accept_city(A,C),true) = true )).

cnf(a1_5,axiom,
    ( ifeq3(any_agent_in_all_proposed_teams(A,L,C),true,accept_leader(A,L),true) = true )).

cnf(a1_7,axiom,
    ( ifeq3(min_number_of_proposed_agents(A,N),true,accept_number(A,N),true) = true )).

cnf(a2_1_5,axiom,
    ( ifeq3(accept_population(A,other,n4),true,ifeq3(accept_population(A,native,n4),true,ifeq3(accept_population(A,muslim,n7),true,ifeq3(accept_population(A,atheist,n65),true,ifeq3(accept_population(A,christian,n20),true,accept_city(A,suffertown),true),true),true),true),true) = true )).

cnf(a2_1_4,axiom,
    ( ifeq3(accept_city(A,suffertown),true,accept_population(A,other,n4),true) = true )).

cnf(a2_1_3,axiom,
    ( ifeq3(accept_city(A,suffertown),true,accept_population(A,native,n4),true) = true )).

cnf(a2_1_2,axiom,
    ( ifeq3(accept_city(A,suffertown),true,accept_population(A,muslim,n7),true) = true )).

cnf(a2_1_1,axiom,
    ( ifeq3(accept_city(A,suffertown),true,accept_population(A,atheist,n65),true) = true )).

cnf(a2_1,axiom,
    ( ifeq3(accept_city(A,suffertown),true,accept_population(A,christian,n20),true) = true )).

cnf(a2_2_5,axiom,
    ( ifeq3(accept_population(A,other,n9),true,ifeq3(accept_population(A,native,n1),true,ifeq3(accept_population(A,muslim,n3),true,ifeq3(accept_population(A,atheist,n54),true,ifeq3(accept_population(A,christian,n23),true,accept_city(A,centraltown),true),true),true),true),true) = true )).

cnf(a2_2_4,axiom,
    ( ifeq3(accept_city(A,centraltown),true,accept_population(A,other,n9),true) = true )).

cnf(a2_2_3,axiom,
    ( ifeq3(accept_city(A,centraltown),true,accept_population(A,native,n1),true) = true )).

cnf(a2_2_2,axiom,
    ( ifeq3(accept_city(A,centraltown),true,accept_population(A,muslim,n3),true) = true )).

cnf(a2_2_1,axiom,
    ( ifeq3(accept_city(A,centraltown),true,accept_population(A,atheist,n54),true) = true )).

cnf(a2_2,axiom,
    ( ifeq3(accept_city(A,centraltown),true,accept_population(A,christian,n23),true) = true )).

cnf(a2_3_5,axiom,
    ( ifeq3(accept_population(A,other,n1),true,ifeq3(accept_population(A,native,n1),true,ifeq3(accept_population(A,muslim,n60),true,ifeq3(accept_population(A,atheist,n30),true,ifeq3(accept_population(A,christian,n8),true,accept_city(A,sunnysideport),true),true),true),true),true) = true )).

cnf(a2_3_4,axiom,
    ( ifeq3(accept_city(A,sunnysideport),true,accept_population(A,other,n1),true) = true )).

cnf(a2_3_3,axiom,
    ( ifeq3(accept_city(A,sunnysideport),true,accept_population(A,native,n1),true) = true )).

cnf(a2_3_2,axiom,
    ( ifeq3(accept_city(A,sunnysideport),true,accept_population(A,muslim,n60),true) = true )).

cnf(a2_3_1,axiom,
    ( ifeq3(accept_city(A,sunnysideport),true,accept_population(A,atheist,n30),true) = true )).

cnf(a2_3,axiom,
    ( ifeq3(accept_city(A,sunnysideport),true,accept_population(A,christian,n8),true) = true )).

cnf(a2_4_5,axiom,
    ( ifeq3(accept_population(A,other,n4),true,ifeq3(accept_population(A,native,n10),true,ifeq3(accept_population(A,muslim,n1),true,ifeq3(accept_population(A,atheist,n70),true,ifeq3(accept_population(A,christian,n15),true,accept_city(A,centrallakecity),true),true),true),true),true) = true )).

cnf(a2_4_4,axiom,
    ( ifeq3(accept_city(A,centrallakecity),true,accept_population(A,other,n4),true) = true )).

cnf(a2_4_3,axiom,
    ( ifeq3(accept_city(A,centrallakecity),true,accept_population(A,native,n10),true) = true )).

cnf(a2_4_2,axiom,
    ( ifeq3(accept_city(A,centrallakecity),true,accept_population(A,muslim,n1),true) = true )).

cnf(a2_4_1,axiom,
    ( ifeq3(accept_city(A,centrallakecity),true,accept_population(A,atheist,n70),true) = true )).

cnf(a2_4,axiom,
    ( ifeq3(accept_city(A,centrallakecity),true,accept_population(A,christian,n15),true) = true )).

cnf(a2_5_5,axiom,
    ( ifeq3(accept_population(A,other,n4),true,ifeq3(accept_population(A,native,n11),true,ifeq3(accept_population(A,muslim,n1),true,ifeq3(accept_population(A,atheist,n68),true,ifeq3(accept_population(A,christian,n16),true,accept_city(A,stjosephburgh),true),true),true),true),true) = true )).

cnf(a2_5_4,axiom,
    ( ifeq3(accept_city(A,stjosephburgh),true,accept_population(A,other,n4),true) = true )).

cnf(a2_5_3,axiom,
    ( ifeq3(accept_city(A,stjosephburgh),true,accept_population(A,native,n11),true) = true )).

cnf(a2_5_2,axiom,
    ( ifeq3(accept_city(A,stjosephburgh),true,accept_population(A,muslim,n1),true) = true )).

cnf(a2_5_1,axiom,
    ( ifeq3(accept_city(A,stjosephburgh),true,accept_population(A,atheist,n68),true) = true )).

cnf(a2_5,axiom,
    ( ifeq3(accept_city(A,stjosephburgh),true,accept_population(A,christian,n16),true) = true )).

cnf(a2_6_5,axiom,
    ( ifeq3(accept_population(A,other,n2),true,ifeq3(accept_population(A,native,n15),true,ifeq3(accept_population(A,muslim,n0),true,ifeq3(accept_population(A,atheist,n70),true,ifeq3(accept_population(A,christian,n13),true,accept_city(A,northport),true),true),true),true),true) = true )).

cnf(a2_6_4,axiom,
    ( ifeq3(accept_city(A,northport),true,accept_population(A,other,n2),true) = true )).

cnf(a2_6_3,axiom,
    ( ifeq3(accept_city(A,northport),true,accept_population(A,native,n15),true) = true )).

cnf(a2_6_2,axiom,
    ( ifeq3(accept_city(A,northport),true,accept_population(A,muslim,n0),true) = true )).

cnf(a2_6_1,axiom,
    ( ifeq3(accept_city(A,northport),true,accept_population(A,atheist,n70),true) = true )).

cnf(a2_6,axiom,
    ( ifeq3(accept_city(A,northport),true,accept_population(A,christian,n13),true) = true )).

cnf(a2_7_5,axiom,
    ( ifeq3(accept_population(A,other,n0),true,ifeq3(accept_population(A,native,n85),true,ifeq3(accept_population(A,muslim,n0),true,ifeq3(accept_population(A,atheist,n12),true,ifeq3(accept_population(A,christian,n3),true,accept_city(A,coastvillage),true),true),true),true),true) = true )).

cnf(a2_7_4,axiom,
    ( ifeq3(accept_city(A,coastvillage),true,accept_population(A,other,n0),true) = true )).

cnf(a2_7_3,axiom,
    ( ifeq3(accept_city(A,coastvillage),true,accept_population(A,native,n85),true) = true )).

cnf(a2_7_2,axiom,
    ( ifeq3(accept_city(A,coastvillage),true,accept_population(A,muslim,n0),true) = true )).

cnf(a2_7_1,axiom,
    ( ifeq3(accept_city(A,coastvillage),true,accept_population(A,atheist,n12),true) = true )).

cnf(a2_7,axiom,
    ( ifeq3(accept_city(A,coastvillage),true,accept_population(A,christian,n3),true) = true )).

cnf(a2_8_5,axiom,
    ( ifeq3(accept_population(A,other,n0),true,ifeq3(accept_population(A,native,n100),true,ifeq3(accept_population(A,muslim,n0),true,ifeq3(accept_population(A,atheist,n0),true,ifeq3(accept_population(A,christian,n0),true,accept_city(A,sunsetpoint),true),true),true),true),true) = true )).

cnf(a2_8_4,axiom,
    ( ifeq3(accept_city(A,sunsetpoint),true,accept_population(A,other,n0),true) = true )).

cnf(a2_8_3,axiom,
    ( ifeq3(accept_city(A,sunsetpoint),true,accept_population(A,native,n100),true) = true )).

cnf(a2_8_2,axiom,
    ( ifeq3(accept_city(A,sunsetpoint),true,accept_population(A,muslim,n0),true) = true )).

cnf(a2_8_1,axiom,
    ( ifeq3(accept_city(A,sunsetpoint),true,accept_population(A,atheist,n0),true) = true )).

cnf(a2_8,axiom,
    ( ifeq3(accept_city(A,sunsetpoint),true,accept_population(A,christian,n0),true) = true )).

cnf(a2_9_5,axiom,
    ( ifeq3(accept_population(A,other,n0),true,ifeq3(accept_population(A,native,n0),true,ifeq3(accept_population(A,muslim,n1),true,ifeq3(accept_population(A,atheist,n75),true,ifeq3(accept_population(A,christian,n24),true,accept_city(A,towna),true),true),true),true),true) = true )).

cnf(a2_9_4,axiom,
    ( ifeq3(accept_city(A,towna),true,accept_population(A,other,n0),true) = true )).

cnf(a2_9_3,axiom,
    ( ifeq3(accept_city(A,towna),true,accept_population(A,native,n0),true) = true )).

cnf(a2_9_2,axiom,
    ( ifeq3(accept_city(A,towna),true,accept_population(A,muslim,n1),true) = true )).

cnf(a2_9_1,axiom,
    ( ifeq3(accept_city(A,towna),true,accept_population(A,atheist,n75),true) = true )).

cnf(a2_9,axiom,
    ( ifeq3(accept_city(A,towna),true,accept_population(A,christian,n24),true) = true )).

cnf(a2_10_5,axiom,
    ( ifeq3(accept_population(A,other,n0),true,ifeq3(accept_population(A,native,n0),true,ifeq3(accept_population(A,muslim,n0),true,ifeq3(accept_population(A,atheist,n75),true,ifeq3(accept_population(A,christian,n25),true,accept_city(A,citya),true),true),true),true),true) = true )).

cnf(a2_10_4,axiom,
    ( ifeq3(accept_city(A,citya),true,accept_population(A,other,n0),true) = true )).

cnf(a2_10_3,axiom,
    ( ifeq3(accept_city(A,citya),true,accept_population(A,native,n0),true) = true )).

cnf(a2_10_2,axiom,
    ( ifeq3(accept_city(A,citya),true,accept_population(A,muslim,n0),true) = true )).

cnf(a2_10_1,axiom,
    ( ifeq3(accept_city(A,citya),true,accept_population(A,atheist,n75),true) = true )).

cnf(a2_10,axiom,
    ( ifeq3(accept_city(A,citya),true,accept_population(A,christian,n25),true) = true )).

cnf(a2_11_5,axiom,
    ( ifeq3(accept_population(A,other,n2),true,ifeq3(accept_population(A,native,n0),true,ifeq3(accept_population(A,muslim,n8),true,ifeq3(accept_population(A,atheist,n70),true,ifeq3(accept_population(A,christian,n20),true,accept_city(A,townb),true),true),true),true),true) = true )).

cnf(a2_11_4,axiom,
    ( ifeq3(accept_city(A,townb),true,accept_population(A,other,n2),true) = true )).

cnf(a2_11_3,axiom,
    ( ifeq3(accept_city(A,townb),true,accept_population(A,native,n0),true) = true )).

cnf(a2_11_2,axiom,
    ( ifeq3(accept_city(A,townb),true,accept_population(A,muslim,n8),true) = true )).

cnf(a2_11_1,axiom,
    ( ifeq3(accept_city(A,townb),true,accept_population(A,atheist,n70),true) = true )).

cnf(a2_11,axiom,
    ( ifeq3(accept_city(A,townb),true,accept_population(A,christian,n20),true) = true )).

cnf(a2_12_5,axiom,
    ( ifeq3(accept_population(A,other,n1),true,ifeq3(accept_population(A,native,n0),true,ifeq3(accept_population(A,muslim,n1),true,ifeq3(accept_population(A,atheist,n78),true,ifeq3(accept_population(A,christian,n20),true,accept_city(A,cityb),true),true),true),true),true) = true )).

cnf(a2_12_4,axiom,
    ( ifeq3(accept_city(A,cityb),true,accept_population(A,other,n1),true) = true )).

cnf(a2_12_3,axiom,
    ( ifeq3(accept_city(A,cityb),true,accept_population(A,native,n0),true) = true )).

cnf(a2_12_2,axiom,
    ( ifeq3(accept_city(A,cityb),true,accept_population(A,muslim,n1),true) = true )).

cnf(a2_12_1,axiom,
    ( ifeq3(accept_city(A,cityb),true,accept_population(A,atheist,n78),true) = true )).

cnf(a2_12,axiom,
    ( ifeq3(accept_city(A,cityb),true,accept_population(A,christian,n20),true) = true )).

cnf(a2_13_5,axiom,
    ( ifeq3(accept_population(A,other,n5),true,ifeq3(accept_population(A,native,n0),true,ifeq3(accept_population(A,muslim,n65),true,ifeq3(accept_population(A,atheist,n30),true,ifeq3(accept_population(A,christian,n0),true,accept_city(A,townc),true),true),true),true),true) = true )).

cnf(a2_13_4,axiom,
    ( ifeq3(accept_city(A,townc),true,accept_population(A,other,n5),true) = true )).

cnf(a2_13_3,axiom,
    ( ifeq3(accept_city(A,townc),true,accept_population(A,native,n0),true) = true )).

cnf(a2_13_2,axiom,
    ( ifeq3(accept_city(A,townc),true,accept_population(A,muslim,n65),true) = true )).

cnf(a2_13_1,axiom,
    ( ifeq3(accept_city(A,townc),true,accept_population(A,atheist,n30),true) = true )).

cnf(a2_13,axiom,
    ( ifeq3(accept_city(A,townc),true,accept_population(A,christian,n0),true) = true )).

cnf(event_1,axiom,
    ( accept_team(countryahumanitarianorganization,countryacivilorganization,cityb,n6) = true )).

cnf(event_2,axiom,
    ( accept_team(countryahumanitarianorganization,countryacivilorganization,towna,n6) = true )).

cnf(event_3,axiom,
    ( accept_team(countryahumanitarianorganization,countryacivilorganization,coastvillage,n6) = true )).

cnf(event_4,axiom,
    ( accept_team(countryahumanitarianorganization,countryafirstaidorganization,coastvillage,n6) = true )).

cnf(event_5,axiom,
    ( the_agent_in_all_proposed_teams(countryahumanitarianorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_6,axiom,
    ( any_agent_in_all_proposed_teams(countryahumanitarianorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_7,axiom,
    ( accept_team(countryccivilorganization,countrybhumanitarianorganization,cityb,n2) = true )).

cnf(event_8,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization,cityb,n4) = true )).

cnf(event_11,axiom,
    ( the_agent_not_in_any_proposed_teams(muslimcountrybhumanitarianorganization,countryacivilorganization,towna) = true )).

cnf(event_12,axiom,
    ( any_agent_in_all_proposed_teams(muslimcountrybhumanitarianorganization,countryacivilorganization,towna) = true )).

cnf(event_13,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization,coastvillage,n5) = true )).

cnf(event_14,axiom,
    ( accept_team(countryamedicalorganization,countryacivilorganization,cityb,n6) = true )).

cnf(event_15,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybhumanitarianorganization,cityb,n4) = true )).

cnf(event_16,axiom,
    ( accept_team(countryafirstaidorganization,countryacivilorganization,cityb,n6) = true )).

cnf(event_17,axiom,
    ( accept_number(countrybhumanitarianorganization,n4) = true )).

cnf(event_18,axiom,
    ( accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n4) = true )).

cnf(event_19,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryacivilorganization,cityb,n6) = true )).

cnf(event_20,axiom,
    ( accept_team(sufferterragovernment,countryacivilorganization,cityb,n6) = true )).

cnf(event_21,axiom,
    ( accept_number(countrybhumanitarianorganization,n1) = true )).

cnf(event_22,axiom,
    ( the_agent_in_all_proposed_teams(countrybhumanitarianorganization,countryacivilorganization,towna) = true )).

cnf(event_23,axiom,
    ( any_agent_in_all_proposed_teams(countrybhumanitarianorganization,countryacivilorganization,towna) = true )).

cnf(event_24,axiom,
    ( accept_team(countrybhumanitarianorganization,christiancountrychumanitarianorganization,coastvillage,n5) = true )).

cnf(event_25,axiom,
    ( accept_team(countrybhumanitarianorganization,christiancountrychumanitarianorganization,coastvillage,n6) = true )).

cnf(event_26,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryacivilorganization,towna,n6) = true )).

cnf(event_27,axiom,
    ( accept_team(countryamedicalorganization,countryacivilorganization,towna,n6) = true )).

cnf(event_28,axiom,
    ( accept_number(countryahumanitarianorganization,n2) = true )).

cnf(event_37,axiom,
    ( the_agent_not_in_any_proposed_teams(countryamedicalorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_38,axiom,
    ( any_agent_in_all_proposed_teams(countryamedicalorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_39,axiom,
    ( accept_team(countryccivilorganization,countrycmedicalorganization,towna,n4) = true )).

cnf(event_41,axiom,
    ( the_agent_in_all_proposed_teams(countryccivilorganization,countryacivilorganization,towna) = true )).

cnf(event_42,axiom,
    ( any_agent_in_all_proposed_teams(countryccivilorganization,countryacivilorganization,towna) = true )).

cnf(event_43,axiom,
    ( accept_team(countrybcivilorganization,countrycmedicalorganization,towna,n4) = true )).

cnf(event_44,axiom,
    ( accept_team(countrybcivilorganization,countrycmedicalorganization,towna,n5) = true )).

cnf(event_45,axiom,
    ( accept_number(countrybcivilorganization,n5) = true )).

cnf(event_46,axiom,
    ( the_agent_in_all_proposed_teams(countrybcivilorganization,countryacivilorganization,towna) = true )).

cnf(event_47,axiom,
    ( any_agent_in_all_proposed_teams(countrybcivilorganization,countryacivilorganization,towna) = true )).

cnf(event_48,axiom,
    ( accept_team(sufferterragovernment,countryacivilorganization,towna,n6) = true )).

cnf(event_49,axiom,
    ( the_agent_in_all_proposed_teams(countrycmedicalorganization,countryacivilorganization,towna) = true )).

cnf(event_50,axiom,
    ( any_agent_in_all_proposed_teams(countrycmedicalorganization,countryacivilorganization,towna) = true )).

cnf(event_51,axiom,
    ( accept_team(countrycmedicalorganization,christiancountrychumanitarianorganization,coastvillage,n5) = true )).

cnf(event_52,axiom,
    ( accept_number(countrycmedicalorganization,n4) = true )).

cnf(event_53,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrycmedicalorganization,towna,n4) = true )).

cnf(event_54,axiom,
    ( accept_number(countrycmedicalorganization,n5) = true )).

cnf(event_55,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrycmedicalorganization,towna,n5) = true )).

cnf(event_56,axiom,
    ( the_agent_in_all_proposed_teams(christiancountrychumanitarianorganization,countryacivilorganization,towna) = true )).

cnf(event_57,axiom,
    ( any_agent_in_all_proposed_teams(christiancountrychumanitarianorganization,countryacivilorganization,towna) = true )).

cnf(event_58,axiom,
    ( accept_team(countryafirstaidorganization,countryacivilorganization,towna,n6) = true )).

cnf(event_59,axiom,
    ( accept_number(countryacivilorganization,n2) = true )).

cnf(event_60,axiom,
    ( accept_team(countryacivilorganization,countryahumanitarianorganization,coastvillage,n5) = true )).

cnf(event_61,axiom,
    ( accept_team(countryacivilorganization,countryahumanitarianorganization,coastvillage,n6) = true )).

cnf(event_62,axiom,
    ( accept_team(countryacivilorganization,countryafirstaidorganization,coastvillage,n6) = true )).

cnf(event_63,axiom,
    ( the_agent_in_all_proposed_teams(countryacivilorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_64,axiom,
    ( any_agent_in_all_proposed_teams(countryacivilorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_65,axiom,
    ( accept_number(countryafirstaidorganization,n2) = true )).

cnf(event_66,axiom,
    ( accept_team(countryafirstaidorganization,countryahumanitarianorganization,coastvillage,n5) = true )).

cnf(event_67,axiom,
    ( accept_team(countryafirstaidorganization,countryahumanitarianorganization,coastvillage,n6) = true )).

cnf(event_68,axiom,
    ( accept_team(countryafirstaidorganization,countryacivilorganization,coastvillage,n6) = true )).

cnf(event_69,axiom,
    ( the_agent_in_all_proposed_teams(countryafirstaidorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_70,axiom,
    ( any_agent_in_all_proposed_teams(countryafirstaidorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_73,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n5) = true )).

cnf(event_74,axiom,
    ( accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,coastvillage,n5) = true )).

cnf(event_75,axiom,
    ( accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,coastvillage,n6) = true )).

cnf(event_76,axiom,
    ( accept_team(countrybcivilorganization,muslimcountrybhumanitarianorganization,townc,n6) = true )).

cnf(event_77,axiom,
    ( accept_team(countrybcivilorganization,countrybhumanitarianorganization,townc,n6) = true )).

cnf(event_78,axiom,
    ( accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,townc,n6) = true )).

cnf(event_79,axiom,
    ( accept_team(countrybcivilorganization,countrycmedicalorganization,townc,n6) = true )).

cnf(event_80,axiom,
    ( the_agent_in_all_proposed_teams(countrybcivilorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_81,axiom,
    ( any_agent_in_all_proposed_teams(countrybcivilorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_82,axiom,
    ( accept_team(sufferterragovernment,countryahumanitarianorganization,coastvillage,n5) = true )).

cnf(event_83,axiom,
    ( accept_team(sufferterragovernment,countryahumanitarianorganization,coastvillage,n6) = true )).

cnf(event_84,axiom,
    ( accept_team(sufferterragovernment,countryacivilorganization,coastvillage,n6) = true )).

cnf(event_85,axiom,
    ( accept_team(sufferterragovernment,countryafirstaidorganization,coastvillage,n6) = true )).

cnf(event_86,axiom,
    ( the_agent_in_all_proposed_teams(sufferterragovernment,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_87,axiom,
    ( any_agent_in_all_proposed_teams(sufferterragovernment,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_88,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n2) = true )).

cnf(event_89,axiom,
    ( accept_number(countryahumanitarianorganization,n5) = true )).

cnf(event_90,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryahumanitarianorganization,coastvillage,n5) = true )).

cnf(event_91,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryahumanitarianorganization,coastvillage,n6) = true )).

cnf(event_92,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryacivilorganization,coastvillage,n6) = true )).

cnf(event_93,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryafirstaidorganization,coastvillage,n6) = true )).

cnf(event_94,axiom,
    ( the_agent_in_all_proposed_teams(christiansufferterrahumanitarianorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_95,axiom,
    ( any_agent_in_all_proposed_teams(christiansufferterrahumanitarianorganization,christiancountrychumanitarianorganization,coastvillage) = true )).

cnf(event_96,axiom,
    ( accept_team(countrycmedicalorganization,christiancountrychumanitarianorganization,coastvillage,n6) = true )).

cnf(event_97,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization,coastvillage,n6) = true )).

cnf(event_98,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n3) = true )).

cnf(event_99,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countryccivilorganization,townc,n4) = true )).

cnf(event_100,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countrybcivilorganization,townc,n6) = true )).

cnf(event_101,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization,townc,n6) = true )).

cnf(event_102,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization,townc,n6) = true )).

cnf(event_103,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countrycmedicalorganization,townc,n6) = true )).

cnf(event_104,axiom,
    ( the_agent_in_all_proposed_teams(muslimcountrybhumanitarianorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_105,axiom,
    ( any_agent_in_all_proposed_teams(muslimcountrybhumanitarianorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_106,axiom,
    ( accept_team(countryacivilorganization,countryahumanitarianorganization,townc,n6) = true )).

cnf(event_107,axiom,
    ( accept_team(countryafirstaidorganization,countryahumanitarianorganization,townc,n6) = true )).

cnf(event_108,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countryccivilorganization,townc,n4) = true )).

cnf(event_109,axiom,
    ( accept_team(christiancountrychumanitarianorganization,muslimcountrybhumanitarianorganization,townc,n6) = true )).

cnf(event_110,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybcivilorganization,townc,n6) = true )).

cnf(event_111,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybhumanitarianorganization,townc,n6) = true )).

cnf(event_112,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrycmedicalorganization,townc,n6) = true )).

cnf(event_113,axiom,
    ( the_agent_in_all_proposed_teams(christiancountrychumanitarianorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_114,axiom,
    ( any_agent_in_all_proposed_teams(christiancountrychumanitarianorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_120,axiom,
    ( the_agent_in_all_proposed_teams(countryccivilorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_121,axiom,
    ( any_agent_in_all_proposed_teams(countryccivilorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_122,axiom,
    ( accept_team(countrybhumanitarianorganization,countrybcivilorganization,townc,n2) = true )).

cnf(event_123,axiom,
    ( accept_team(countrybhumanitarianorganization,muslimcountrybhumanitarianorganization,townc,n6) = true )).

cnf(event_124,axiom,
    ( accept_team(sufferterragovernment,countryahumanitarianorganization,townc,n6) = true )).

cnf(event_125,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryahumanitarianorganization,townc,n6) = true )).

cnf(event_126,axiom,
    ( accept_team(countryamedicalorganization,countryahumanitarianorganization,townc,n6) = true )).

cnf(event_127,axiom,
    ( accept_team(countrycmedicalorganization,countryccivilorganization,townc,n4) = true )).

cnf(event_128,axiom,
    ( accept_team(countrycmedicalorganization,muslimcountrybhumanitarianorganization,townc,n6) = true )).

cnf(event_129,axiom,
    ( accept_team(countrycmedicalorganization,countrybcivilorganization,townc,n6) = true )).

cnf(event_130,axiom,
    ( accept_team(countrycmedicalorganization,countrybhumanitarianorganization,townc,n6) = true )).

cnf(event_131,axiom,
    ( accept_team(countrycmedicalorganization,christiancountrychumanitarianorganization,townc,n6) = true )).

cnf(event_132,axiom,
    ( the_agent_in_all_proposed_teams(countrycmedicalorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_133,axiom,
    ( any_agent_in_all_proposed_teams(countrycmedicalorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_134,axiom,
    ( accept_team(countrybhumanitarianorganization,countrybcivilorganization,townc,n6) = true )).

cnf(event_135,axiom,
    ( accept_team(countrybhumanitarianorganization,christiancountrychumanitarianorganization,townc,n6) = true )).

cnf(event_136,axiom,
    ( accept_team(countrybhumanitarianorganization,countrycmedicalorganization,townc,n6) = true )).

cnf(event_137,axiom,
    ( the_agent_in_all_proposed_teams(countrybhumanitarianorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_138,axiom,
    ( any_agent_in_all_proposed_teams(countrybhumanitarianorganization,countryahumanitarianorganization,townc) = true )).

cnf(event_139,axiom,
    ( accept_team(christiansufferterrahumanitarianorganization,countryahumanitarianorganization,cityb,n6) = true )).

cnf(event_140,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n6) = true )).

cnf(event_141,axiom,
    ( the_agent_in_all_proposed_teams(christiansufferterrahumanitarianorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_142,axiom,
    ( any_agent_in_all_proposed_teams(christiansufferterrahumanitarianorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_145,axiom,
    ( accept_team(sufferterragovernment,countryahumanitarianorganization,cityb,n6) = true )).

cnf(event_146,axiom,
    ( the_agent_in_all_proposed_teams(sufferterragovernment,countrybhumanitarianorganization,cityb) = true )).

cnf(event_147,axiom,
    ( any_agent_in_all_proposed_teams(sufferterragovernment,countrybhumanitarianorganization,cityb) = true )).

cnf(event_148,axiom,
    ( accept_team(countryafirstaidorganization,countryahumanitarianorganization,cityb,n6) = true )).

cnf(event_149,axiom,
    ( accept_number(countryafirstaidorganization,n6) = true )).

cnf(event_150,axiom,
    ( the_agent_in_all_proposed_teams(countryafirstaidorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_151,axiom,
    ( any_agent_in_all_proposed_teams(countryafirstaidorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_152,axiom,
    ( accept_team(countryafirstaidorganization,sufferterragovernment,towna,n5) = true )).

cnf(event_153,axiom,
    ( accept_team(countryafirstaidorganization,sufferterragovernment,towna,n6) = true )).

cnf(event_154,axiom,
    ( accept_team(countrycmedicalorganization,countrybhumanitarianorganization,cityb,n5) = true )).

cnf(event_155,axiom,
    ( accept_team(countrycmedicalorganization,countrybhumanitarianorganization,cityb,n6) = true )).

cnf(event_156,axiom,
    ( accept_team(countryamedicalorganization,countryahumanitarianorganization,cityb,n6) = true )).

cnf(event_157,axiom,
    ( accept_number(countryamedicalorganization,n6) = true )).

cnf(event_158,axiom,
    ( the_agent_in_all_proposed_teams(countryamedicalorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_159,axiom,
    ( any_agent_in_all_proposed_teams(countryamedicalorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_160,axiom,
    ( accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n5) = true )).

cnf(event_161,axiom,
    ( accept_team(countrybcivilorganization,countrybhumanitarianorganization,cityb,n6) = true )).

cnf(event_162,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybhumanitarianorganization,cityb,n5) = true )).

cnf(event_163,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybhumanitarianorganization,cityb,n6) = true )).

cnf(event_164,axiom,
    ( the_agent_in_all_proposed_teams(countryahumanitarianorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_165,axiom,
    ( any_agent_in_all_proposed_teams(countryahumanitarianorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_166,axiom,
    ( accept_number(countryahumanitarianorganization,n6) = true )).

cnf(event_167,axiom,
    ( accept_team(countryacivilorganization,countryahumanitarianorganization,cityb,n6) = true )).

cnf(event_168,axiom,
    ( accept_number(countryacivilorganization,n6) = true )).

cnf(event_169,axiom,
    ( the_agent_in_all_proposed_teams(countryacivilorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_170,axiom,
    ( any_agent_in_all_proposed_teams(countryacivilorganization,countrybhumanitarianorganization,cityb) = true )).

cnf(event_171,axiom,
    ( accept_number(countrybhumanitarianorganization,n5) = true )).

cnf(event_172,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization,cityb,n5) = true )).

cnf(event_173,axiom,
    ( accept_team(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization,cityb,n6) = true )).

cnf(event_183,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n2) = true )).

cnf(event_188,axiom,
    ( accept_team(countryamedicalorganization,sufferterragovernment,towna,n5) = true )).

cnf(event_189,axiom,
    ( accept_team(countryamedicalorganization,sufferterragovernment,towna,n6) = true )).

cnf(event_190,axiom,
    ( accept_team(countryahumanitarianorganization,sufferterragovernment,towna,n5) = true )).

cnf(event_191,axiom,
    ( accept_team(countryahumanitarianorganization,sufferterragovernment,towna,n6) = true )).

cnf(event_192,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n2) = true )).

cnf(event_193,axiom,
    ( accept_team(countrybhumanitarianorganization,countrybcivilorganization,towna,n3) = true )).

cnf(event_194,axiom,
    ( accept_team(countrybhumanitarianorganization,christiancountrychumanitarianorganization,towna,n3) = true )).

cnf(event_195,axiom,
    ( accept_team(countrybhumanitarianorganization,countrycmedicalorganization,towna,n3) = true )).

cnf(event_196,axiom,
    ( accept_team(countrybhumanitarianorganization,countryccivilorganization,towna,n3) = true )).

cnf(event_197,axiom,
    ( accept_number(countrybhumanitarianorganization,n2) = true )).

cnf(event_198,axiom,
    ( accept_team(countrybhumanitarianorganization,countrybcivilorganization,towna,n6) = true )).

cnf(event_199,axiom,
    ( accept_team(countrybhumanitarianorganization,christiancountrychumanitarianorganization,towna,n6) = true )).

cnf(event_200,axiom,
    ( accept_team(countrybhumanitarianorganization,countrycmedicalorganization,towna,n6) = true )).

cnf(event_201,axiom,
    ( the_agent_in_all_proposed_teams(countrybhumanitarianorganization,sufferterragovernment,towna) = true )).

cnf(event_202,axiom,
    ( any_agent_in_all_proposed_teams(countrybhumanitarianorganization,sufferterragovernment,towna) = true )).

cnf(event_203,axiom,
    ( accept_number(countrybcivilorganization,n2) = true )).

cnf(event_204,axiom,
    ( accept_number(countrybcivilorganization,n3) = true )).

cnf(event_205,axiom,
    ( accept_team(countrybcivilorganization,countryccivilorganization,towna,n4) = true )).

cnf(event_206,axiom,
    ( accept_team(countrybcivilorganization,countrybhumanitarianorganization,towna,n6) = true )).

cnf(event_207,axiom,
    ( accept_team(countrybcivilorganization,christiancountrychumanitarianorganization,towna,n6) = true )).

cnf(event_208,axiom,
    ( accept_team(countrybcivilorganization,countrycmedicalorganization,towna,n6) = true )).

cnf(event_209,axiom,
    ( the_agent_in_all_proposed_teams(countrybcivilorganization,sufferterragovernment,towna) = true )).

cnf(event_210,axiom,
    ( any_agent_in_all_proposed_teams(countrybcivilorganization,sufferterragovernment,towna) = true )).

cnf(event_211,axiom,
    ( accept_number(sufferterragovernment,n2) = true )).

cnf(event_214,axiom,
    ( accept_number(countryccivilorganization,n2) = true )).

cnf(event_215,axiom,
    ( accept_number(countryccivilorganization,n3) = true )).

cnf(event_220,axiom,
    ( the_agent_in_all_proposed_teams(countryccivilorganization,sufferterragovernment,towna) = true )).

cnf(event_221,axiom,
    ( any_agent_in_all_proposed_teams(countryccivilorganization,sufferterragovernment,towna) = true )).

cnf(event_222,axiom,
    ( accept_number(sufferterragovernment,n5) = true )).

cnf(event_223,axiom,
    ( accept_team(countryacivilorganization,sufferterragovernment,towna,n5) = true )).

cnf(event_224,axiom,
    ( accept_number(sufferterragovernment,n6) = true )).

cnf(event_225,axiom,
    ( accept_team(countryacivilorganization,sufferterragovernment,towna,n6) = true )).

cnf(event_226,axiom,
    ( accept_number(countrycmedicalorganization,n2) = true )).

cnf(event_227,axiom,
    ( accept_number(countrycmedicalorganization,n3) = true )).

cnf(event_228,axiom,
    ( accept_team(countrycmedicalorganization,countryccivilorganization,towna,n4) = true )).

cnf(event_229,axiom,
    ( accept_team(countrycmedicalorganization,countrybcivilorganization,towna,n6) = true )).

cnf(event_230,axiom,
    ( accept_team(countrycmedicalorganization,countrybhumanitarianorganization,towna,n6) = true )).

cnf(event_231,axiom,
    ( accept_team(countrycmedicalorganization,christiancountrychumanitarianorganization,towna,n6) = true )).

cnf(event_232,axiom,
    ( the_agent_in_all_proposed_teams(countrycmedicalorganization,sufferterragovernment,towna) = true )).

cnf(event_233,axiom,
    ( any_agent_in_all_proposed_teams(countrycmedicalorganization,sufferterragovernment,towna) = true )).

cnf(event_234,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n3) = true )).

cnf(event_235,axiom,
    ( accept_number(countryccivilorganization,n4) = true )).

cnf(event_236,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countryccivilorganization,towna,n4) = true )).

cnf(event_237,axiom,
    ( accept_number(countrybcivilorganization,n6) = true )).

cnf(event_238,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybcivilorganization,towna,n6) = true )).

cnf(event_239,axiom,
    ( accept_number(countrybhumanitarianorganization,n6) = true )).

cnf(event_240,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrybhumanitarianorganization,towna,n6) = true )).

cnf(event_241,axiom,
    ( accept_team(christiancountrychumanitarianorganization,countrycmedicalorganization,towna,n6) = true )).

cnf(event_242,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n6) = true )).

cnf(event_243,axiom,
    ( the_agent_in_all_proposed_teams(christiancountrychumanitarianorganization,sufferterragovernment,towna) = true )).

cnf(event_244,axiom,
    ( any_agent_in_all_proposed_teams(christiancountrychumanitarianorganization,sufferterragovernment,towna) = true )).

cnf(event_245,axiom,
    ( accept_number(countrycmedicalorganization,n6) = true )).

cnf(event_247,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n6) = true )).

cnf(event_248,axiom,
    ( the_agent_not_in_any_proposed_teams(muslimcountrybhumanitarianorganization,sufferterragovernment,towna) = true )).

cnf(event_249,axiom,
    ( any_agent_in_all_proposed_teams(muslimcountrybhumanitarianorganization,sufferterragovernment,towna) = true )).

cnf(deduced_1,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,countryacivilorganization) = true )).

cnf(deduced_2,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,muslimcountrybhumanitarianorganization) = true )).

cnf(deduced_3,axiom,
    ( accept_population(christiancountrychumanitarianorganization,other,n5) = true )).

cnf(deduced_4,axiom,
    ( accept_population(christiancountrychumanitarianorganization,muslim,n65) = true )).

cnf(deduced_5,axiom,
    ( accept_population(christiancountrychumanitarianorganization,christian,n0) = true )).

cnf(deduced_6,axiom,
    ( accept_population(christiancountrychumanitarianorganization,atheist,n30) = true )).

cnf(deduced_7,axiom,
    ( accept_city(christiancountrychumanitarianorganization,townc) = true )).

cnf(deduced_8,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,countryahumanitarianorganization) = true )).

cnf(deduced_9,axiom,
    ( accept_population(christiancountrychumanitarianorganization,other,n1) = true )).

cnf(deduced_10,axiom,
    ( accept_population(christiancountrychumanitarianorganization,christian,n20) = true )).

cnf(deduced_11,axiom,
    ( accept_population(christiancountrychumanitarianorganization,atheist,n78) = true )).

cnf(deduced_12,axiom,
    ( accept_city(christiancountrychumanitarianorganization,cityb) = true )).

cnf(deduced_14,axiom,
    ( accept_leader(countryamedicalorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_15,axiom,
    ( accept_population(countryamedicalorganization,other,n5) = true )).

cnf(deduced_16,axiom,
    ( accept_population(countryamedicalorganization,muslim,n65) = true )).

cnf(deduced_17,axiom,
    ( accept_population(countryamedicalorganization,christian,n0) = true )).

cnf(deduced_18,axiom,
    ( accept_population(countryamedicalorganization,atheist,n30) = true )).

cnf(deduced_19,axiom,
    ( accept_leader(countryamedicalorganization,countryahumanitarianorganization) = true )).

cnf(deduced_20,axiom,
    ( accept_city(countryamedicalorganization,townc) = true )).

cnf(deduced_21,axiom,
    ( accept_leader(countryahumanitarianorganization,countryamedicalorganization) = true )).

cnf(deduced_22,axiom,
    ( accept_leader(countryahumanitarianorganization,countryafirstaidorganization) = true )).

cnf(deduced_23,axiom,
    ( accept_population(countryahumanitarianorganization,native,n85) = true )).

cnf(deduced_24,axiom,
    ( accept_population(countryahumanitarianorganization,muslim,n0) = true )).

cnf(deduced_25,axiom,
    ( accept_population(countryahumanitarianorganization,christian,n3) = true )).

cnf(deduced_26,axiom,
    ( accept_population(countryahumanitarianorganization,atheist,n12) = true )).

cnf(deduced_27,axiom,
    ( accept_city(countryahumanitarianorganization,coastvillage) = true )).

cnf(deduced_28,axiom,
    ( accept_leader(countryahumanitarianorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_29,axiom,
    ( accept_leader(countryafirstaidorganization,countryamedicalorganization) = true )).

cnf(deduced_30,axiom,
    ( accept_leader(christiansufferterrahumanitarianorganization,countryamedicalorganization) = true )).

cnf(deduced_31,axiom,
    ( accept_leader(countrycmedicalorganization,countryacivilorganization) = true )).

cnf(deduced_32,axiom,
    ( accept_population(countrycmedicalorganization,native,n85) = true )).

cnf(deduced_33,axiom,
    ( accept_population(countrycmedicalorganization,muslim,n0) = true )).

cnf(deduced_34,axiom,
    ( accept_population(countrycmedicalorganization,christian,n3) = true )).

cnf(deduced_35,axiom,
    ( accept_population(countrycmedicalorganization,atheist,n12) = true )).

cnf(deduced_36,axiom,
    ( accept_city(countrycmedicalorganization,coastvillage) = true )).

cnf(deduced_37,axiom,
    ( accept_leader(countrybcivilorganization,countryacivilorganization) = true )).

cnf(deduced_38,axiom,
    ( accept_population(countrybcivilorganization,native,n85) = true )).

cnf(deduced_39,axiom,
    ( accept_population(countrybcivilorganization,muslim,n0) = true )).

cnf(deduced_40,axiom,
    ( accept_population(countrybcivilorganization,christian,n3) = true )).

cnf(deduced_41,axiom,
    ( accept_population(countrybcivilorganization,atheist,n12) = true )).

cnf(deduced_42,axiom,
    ( accept_city(countrybcivilorganization,coastvillage) = true )).

cnf(deduced_43,axiom,
    ( accept_leader(countrybcivilorganization,muslimcountrybhumanitarianorganization) = true )).

cnf(deduced_44,axiom,
    ( accept_population(countrybcivilorganization,other,n5) = true )).

cnf(deduced_45,axiom,
    ( accept_population(countrybcivilorganization,muslim,n65) = true )).

cnf(deduced_46,axiom,
    ( accept_population(countrybcivilorganization,christian,n0) = true )).

cnf(deduced_47,axiom,
    ( accept_population(countrybcivilorganization,atheist,n30) = true )).

cnf(deduced_48,axiom,
    ( accept_city(countrybcivilorganization,townc) = true )).

cnf(deduced_49,axiom,
    ( accept_leader(countrybcivilorganization,countryahumanitarianorganization) = true )).

cnf(deduced_50,axiom,
    ( accept_population(countrybcivilorganization,other,n1) = true )).

cnf(deduced_51,axiom,
    ( accept_population(countrybcivilorganization,christian,n20) = true )).

cnf(deduced_52,axiom,
    ( accept_population(countrybcivilorganization,atheist,n78) = true )).

cnf(deduced_53,axiom,
    ( accept_city(countrybcivilorganization,cityb) = true )).

cnf(deduced_54,axiom,
    ( accept_leader(sufferterragovernment,countryamedicalorganization) = true )).

cnf(deduced_55,axiom,
    ( accept_population(sufferterragovernment,christian,n24) = true )).

cnf(deduced_56,axiom,
    ( accept_population(sufferterragovernment,atheist,n75) = true )).

cnf(deduced_57,axiom,
    ( accept_city(sufferterragovernment,towna) = true )).

cnf(deduced_59,axiom,
    ( accept_population(countryccivilorganization,other,n1) = true )).

cnf(deduced_60,axiom,
    ( accept_population(countryccivilorganization,christian,n20) = true )).

cnf(deduced_61,axiom,
    ( accept_population(countryccivilorganization,atheist,n78) = true )).

cnf(deduced_63,axiom,
    ( accept_leader(countryccivilorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_64,axiom,
    ( accept_city(countryccivilorganization,cityb) = true )).

cnf(deduced_65,axiom,
    ( accept_leader(countryccivilorganization,countryacivilorganization) = true )).

cnf(deduced_66,axiom,
    ( accept_leader(countryacivilorganization,countryamedicalorganization) = true )).

cnf(deduced_67,axiom,
    ( accept_leader(countryacivilorganization,countryafirstaidorganization) = true )).

cnf(deduced_68,axiom,
    ( accept_population(countryacivilorganization,native,n85) = true )).

cnf(deduced_69,axiom,
    ( accept_population(countryacivilorganization,muslim,n0) = true )).

cnf(deduced_70,axiom,
    ( accept_population(countryacivilorganization,christian,n3) = true )).

cnf(deduced_71,axiom,
    ( accept_population(countryacivilorganization,atheist,n12) = true )).

cnf(deduced_72,axiom,
    ( accept_city(countryacivilorganization,coastvillage) = true )).

cnf(deduced_73,axiom,
    ( accept_leader(countryacivilorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_74,axiom,
    ( accept_population(countryacivilorganization,other,n5) = true )).

cnf(deduced_75,axiom,
    ( accept_population(countryacivilorganization,muslim,n65) = true )).

cnf(deduced_76,axiom,
    ( accept_population(countryacivilorganization,christian,n0) = true )).

cnf(deduced_77,axiom,
    ( accept_population(countryacivilorganization,atheist,n30) = true )).

cnf(deduced_78,axiom,
    ( accept_leader(countryacivilorganization,countryahumanitarianorganization) = true )).

cnf(deduced_79,axiom,
    ( accept_city(countryacivilorganization,townc) = true )).

cnf(deduced_80,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,christian,n24) = true )).

cnf(deduced_81,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,atheist,n75) = true )).

cnf(deduced_83,axiom,
    ( accept_city(christiansufferterrahumanitarianorganization,towna) = true )).

cnf(deduced_84,axiom,
    ( accept_leader(christiansufferterrahumanitarianorganization,countryafirstaidorganization) = true )).

cnf(deduced_85,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,other,n0) = true )).

cnf(deduced_86,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,native,n85) = true )).

cnf(deduced_87,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,muslim,n0) = true )).

cnf(deduced_88,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,christian,n3) = true )).

cnf(deduced_89,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,atheist,n12) = true )).

cnf(deduced_90,axiom,
    ( accept_city(christiansufferterrahumanitarianorganization,coastvillage) = true )).

cnf(deduced_91,axiom,
    ( accept_leader(christiansufferterrahumanitarianorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_92,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,countryacivilorganization) = true )).

cnf(deduced_93,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,other,n0) = true )).

cnf(deduced_94,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,native,n85) = true )).

cnf(deduced_95,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,muslim,n0) = true )).

cnf(deduced_96,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,christian,n3) = true )).

cnf(deduced_97,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,atheist,n12) = true )).

cnf(deduced_98,axiom,
    ( accept_city(muslimcountrybhumanitarianorganization,coastvillage) = true )).

cnf(deduced_99,axiom,
    ( accept_population(countryafirstaidorganization,native,n85) = true )).

cnf(deduced_100,axiom,
    ( accept_population(countryafirstaidorganization,muslim,n0) = true )).

cnf(deduced_101,axiom,
    ( accept_population(countryafirstaidorganization,christian,n3) = true )).

cnf(deduced_102,axiom,
    ( accept_population(countryafirstaidorganization,atheist,n12) = true )).

cnf(deduced_103,axiom,
    ( accept_city(countryafirstaidorganization,coastvillage) = true )).

cnf(deduced_104,axiom,
    ( accept_leader(countryafirstaidorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_105,axiom,
    ( accept_population(countryafirstaidorganization,other,n5) = true )).

cnf(deduced_106,axiom,
    ( accept_population(countryafirstaidorganization,muslim,n65) = true )).

cnf(deduced_107,axiom,
    ( accept_population(countryafirstaidorganization,christian,n0) = true )).

cnf(deduced_108,axiom,
    ( accept_population(countryafirstaidorganization,atheist,n30) = true )).

cnf(deduced_109,axiom,
    ( accept_leader(countryafirstaidorganization,countryahumanitarianorganization) = true )).

cnf(deduced_110,axiom,
    ( accept_city(countryafirstaidorganization,townc) = true )).

cnf(deduced_111,axiom,
    ( accept_leader(countryafirstaidorganization,countryacivilorganization) = true )).

cnf(deduced_112,axiom,
    ( accept_population(countryafirstaidorganization,other,n1) = true )).

cnf(deduced_113,axiom,
    ( accept_population(countryafirstaidorganization,christian,n20) = true )).

cnf(deduced_114,axiom,
    ( accept_population(countryafirstaidorganization,atheist,n78) = true )).

cnf(deduced_115,axiom,
    ( accept_city(countryafirstaidorganization,cityb) = true )).

cnf(deduced_116,axiom,
    ( accept_leader(countryafirstaidorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_117,axiom,
    ( accept_leader(countrybhumanitarianorganization,countryacivilorganization) = true )).

cnf(deduced_118,axiom,
    ( accept_population(countrybhumanitarianorganization,native,n85) = true )).

cnf(deduced_119,axiom,
    ( accept_population(countrybhumanitarianorganization,muslim,n0) = true )).

cnf(deduced_120,axiom,
    ( accept_population(countrybhumanitarianorganization,christian,n3) = true )).

cnf(deduced_121,axiom,
    ( accept_population(countrybhumanitarianorganization,atheist,n12) = true )).

cnf(deduced_122,axiom,
    ( accept_city(countrybhumanitarianorganization,coastvillage) = true )).

cnf(deduced_123,axiom,
    ( accept_leader(countrybhumanitarianorganization,muslimcountrybhumanitarianorganization) = true )).

cnf(deduced_124,axiom,
    ( accept_population(countrybhumanitarianorganization,other,n5) = true )).

cnf(deduced_125,axiom,
    ( accept_population(countrybhumanitarianorganization,muslim,n65) = true )).

cnf(deduced_126,axiom,
    ( accept_population(countrybhumanitarianorganization,christian,n0) = true )).

cnf(deduced_127,axiom,
    ( accept_population(countrybhumanitarianorganization,atheist,n30) = true )).

cnf(deduced_128,axiom,
    ( accept_city(countrybhumanitarianorganization,townc) = true )).

cnf(deduced_129,axiom,
    ( accept_leader(countrybhumanitarianorganization,countryahumanitarianorganization) = true )).

cnf(deduced_130,axiom,
    ( accept_leader(countrybhumanitarianorganization,countryccivilorganization) = true )).

cnf(deduced_131,axiom,
    ( accept_leader(countrybhumanitarianorganization,countrybcivilorganization) = true )).

cnf(deduced_132,axiom,
    ( accept_leader(countrybhumanitarianorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_133,axiom,
    ( accept_leader(countrybhumanitarianorganization,countrycmedicalorganization) = true )).

cnf(deduced_134,axiom,
    ( accept_population(countrybhumanitarianorganization,other,n0) = true )).

cnf(deduced_135,axiom,
    ( accept_population(countrybhumanitarianorganization,native,n0) = true )).

cnf(deduced_136,axiom,
    ( accept_population(countrybhumanitarianorganization,muslim,n1) = true )).

cnf(deduced_137,axiom,
    ( accept_population(countrybhumanitarianorganization,christian,n24) = true )).

cnf(deduced_138,axiom,
    ( accept_population(countrybhumanitarianorganization,atheist,n75) = true )).

cnf(deduced_139,axiom,
    ( accept_city(countrybhumanitarianorganization,towna) = true )).

cnf(deduced_140,axiom,
    ( accept_leader(countrybhumanitarianorganization,sufferterragovernment) = true )).

cnf(deduced_141,axiom,
    ( accept_leader(sufferterragovernment,countryafirstaidorganization) = true )).

cnf(deduced_142,axiom,
    ( accept_population(sufferterragovernment,other,n0) = true )).

cnf(deduced_143,axiom,
    ( accept_population(sufferterragovernment,native,n85) = true )).

cnf(deduced_144,axiom,
    ( accept_population(sufferterragovernment,muslim,n0) = true )).

cnf(deduced_145,axiom,
    ( accept_population(sufferterragovernment,christian,n3) = true )).

cnf(deduced_146,axiom,
    ( accept_population(sufferterragovernment,atheist,n12) = true )).

cnf(deduced_147,axiom,
    ( accept_city(sufferterragovernment,coastvillage) = true )).

cnf(deduced_148,axiom,
    ( accept_leader(sufferterragovernment,christiancountrychumanitarianorganization) = true )).

cnf(deduced_149,axiom,
    ( accept_population(sufferterragovernment,other,n5) = true )).

cnf(deduced_150,axiom,
    ( accept_population(sufferterragovernment,muslim,n65) = true )).

cnf(deduced_151,axiom,
    ( accept_population(sufferterragovernment,christian,n0) = true )).

cnf(deduced_152,axiom,
    ( accept_population(sufferterragovernment,atheist,n30) = true )).

cnf(deduced_153,axiom,
    ( accept_leader(sufferterragovernment,countryahumanitarianorganization) = true )).

cnf(deduced_154,axiom,
    ( accept_city(sufferterragovernment,townc) = true )).

cnf(deduced_155,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,countryccivilorganization) = true )).

cnf(deduced_156,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,countrybcivilorganization) = true )).

cnf(deduced_157,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,other,n5) = true )).

cnf(deduced_158,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,muslim,n65) = true )).

cnf(deduced_159,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,christian,n0) = true )).

cnf(deduced_160,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,atheist,n30) = true )).

cnf(deduced_161,axiom,
    ( accept_leader(christiansufferterrahumanitarianorganization,countryahumanitarianorganization) = true )).

cnf(deduced_162,axiom,
    ( accept_city(christiansufferterrahumanitarianorganization,townc) = true )).

cnf(deduced_163,axiom,
    ( accept_leader(christiansufferterrahumanitarianorganization,countryacivilorganization) = true )).

cnf(deduced_164,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n5) = true )).

cnf(deduced_165,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n4) = true )).

cnf(deduced_166,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n3) = true )).

cnf(deduced_167,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n2) = true )).

cnf(deduced_168,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n1) = true )).

cnf(deduced_169,axiom,
    ( accept_number(christiansufferterrahumanitarianorganization,n6) = true )).

cnf(deduced_170,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,other,n1) = true )).

cnf(deduced_171,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,native,n0) = true )).

cnf(deduced_172,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,muslim,n1) = true )).

cnf(deduced_173,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,christian,n20) = true )).

cnf(deduced_174,axiom,
    ( accept_population(christiansufferterrahumanitarianorganization,atheist,n78) = true )).

cnf(deduced_175,axiom,
    ( accept_city(christiansufferterrahumanitarianorganization,cityb) = true )).

cnf(deduced_176,axiom,
    ( accept_leader(christiansufferterrahumanitarianorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_177,axiom,
    ( accept_population(countryccivilorganization,other,n5) = true )).

cnf(deduced_178,axiom,
    ( accept_population(countryccivilorganization,muslim,n65) = true )).

cnf(deduced_179,axiom,
    ( accept_population(countryccivilorganization,christian,n0) = true )).

cnf(deduced_180,axiom,
    ( accept_population(countryccivilorganization,atheist,n30) = true )).

cnf(deduced_181,axiom,
    ( accept_city(countryccivilorganization,townc) = true )).

cnf(deduced_182,axiom,
    ( accept_leader(countryccivilorganization,countryahumanitarianorganization) = true )).

cnf(deduced_183,axiom,
    ( accept_leader(countrycmedicalorganization,muslimcountrybhumanitarianorganization) = true )).

cnf(deduced_184,axiom,
    ( accept_population(countrycmedicalorganization,other,n5) = true )).

cnf(deduced_185,axiom,
    ( accept_population(countrycmedicalorganization,muslim,n65) = true )).

cnf(deduced_186,axiom,
    ( accept_population(countrycmedicalorganization,christian,n0) = true )).

cnf(deduced_187,axiom,
    ( accept_population(countrycmedicalorganization,atheist,n30) = true )).

cnf(deduced_188,axiom,
    ( accept_city(countrycmedicalorganization,townc) = true )).

cnf(deduced_189,axiom,
    ( accept_leader(countrycmedicalorganization,countryahumanitarianorganization) = true )).

cnf(deduced_190,axiom,
    ( accept_population(countrycmedicalorganization,other,n1) = true )).

cnf(deduced_191,axiom,
    ( accept_population(countrycmedicalorganization,christian,n20) = true )).

cnf(deduced_192,axiom,
    ( accept_population(countrycmedicalorganization,atheist,n78) = true )).

cnf(deduced_193,axiom,
    ( accept_city(countrycmedicalorganization,cityb) = true )).

cnf(deduced_194,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,countrycmedicalorganization) = true )).

cnf(deduced_195,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_196,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,other,n5) = true )).

cnf(deduced_197,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,muslim,n65) = true )).

cnf(deduced_198,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,christian,n0) = true )).

cnf(deduced_199,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,atheist,n30) = true )).

cnf(deduced_200,axiom,
    ( accept_city(muslimcountrybhumanitarianorganization,townc) = true )).

cnf(deduced_201,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,countryahumanitarianorganization) = true )).

cnf(deduced_202,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,other,n1) = true )).

cnf(deduced_203,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,native,n0) = true )).

cnf(deduced_204,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,muslim,n1) = true )).

cnf(deduced_205,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,christian,n20) = true )).

cnf(deduced_206,axiom,
    ( accept_population(muslimcountrybhumanitarianorganization,atheist,n78) = true )).

cnf(deduced_207,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_208,axiom,
    ( accept_city(muslimcountrybhumanitarianorganization,cityb) = true )).

cnf(deduced_209,axiom,
    ( accept_leader(countryamedicalorganization,countryacivilorganization) = true )).

cnf(deduced_210,axiom,
    ( accept_population(countryamedicalorganization,other,n1) = true )).

cnf(deduced_211,axiom,
    ( accept_population(countryamedicalorganization,christian,n20) = true )).

cnf(deduced_212,axiom,
    ( accept_population(countryamedicalorganization,atheist,n78) = true )).

cnf(deduced_213,axiom,
    ( accept_city(countryamedicalorganization,cityb) = true )).

cnf(deduced_214,axiom,
    ( accept_leader(countryamedicalorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_215,axiom,
    ( accept_number(countryamedicalorganization,n5) = true )).

cnf(deduced_216,axiom,
    ( accept_number(countryamedicalorganization,n4) = true )).

cnf(deduced_217,axiom,
    ( accept_number(countryamedicalorganization,n3) = true )).

cnf(deduced_218,axiom,
    ( accept_number(countryamedicalorganization,n2) = true )).

cnf(deduced_219,axiom,
    ( accept_number(countryamedicalorganization,n1) = true )).

cnf(deduced_220,axiom,
    ( accept_population(countryamedicalorganization,other,n0) = true )).

cnf(deduced_221,axiom,
    ( accept_population(countryamedicalorganization,native,n0) = true )).

cnf(deduced_222,axiom,
    ( accept_population(countryamedicalorganization,muslim,n1) = true )).

cnf(deduced_223,axiom,
    ( accept_population(countryamedicalorganization,christian,n24) = true )).

cnf(deduced_224,axiom,
    ( accept_population(countryamedicalorganization,atheist,n75) = true )).

cnf(deduced_225,axiom,
    ( accept_number(countryamedicalorganization,n6) = true )).

cnf(deduced_226,axiom,
    ( accept_leader(countryamedicalorganization,sufferterragovernment) = true )).

cnf(deduced_227,axiom,
    ( accept_city(countryamedicalorganization,towna) = true )).

cnf(deduced_228,axiom,
    ( accept_population(countryacivilorganization,other,n1) = true )).

cnf(deduced_229,axiom,
    ( accept_population(countryacivilorganization,christian,n20) = true )).

cnf(deduced_230,axiom,
    ( accept_population(countryacivilorganization,atheist,n78) = true )).

cnf(deduced_231,axiom,
    ( accept_city(countryacivilorganization,cityb) = true )).

cnf(deduced_232,axiom,
    ( accept_leader(countryacivilorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_233,axiom,
    ( accept_leader(countryahumanitarianorganization,countryacivilorganization) = true )).

cnf(deduced_234,axiom,
    ( accept_population(countryahumanitarianorganization,other,n1) = true )).

cnf(deduced_235,axiom,
    ( accept_population(countryahumanitarianorganization,christian,n20) = true )).

cnf(deduced_236,axiom,
    ( accept_population(countryahumanitarianorganization,atheist,n78) = true )).

cnf(deduced_237,axiom,
    ( accept_city(countryahumanitarianorganization,cityb) = true )).

cnf(deduced_238,axiom,
    ( accept_leader(countryahumanitarianorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_239,axiom,
    ( accept_number(countryahumanitarianorganization,n5) = true )).

cnf(deduced_240,axiom,
    ( accept_number(countryahumanitarianorganization,n4) = true )).

cnf(deduced_241,axiom,
    ( accept_number(countryahumanitarianorganization,n3) = true )).

cnf(deduced_242,axiom,
    ( accept_number(countryahumanitarianorganization,n2) = true )).

cnf(deduced_243,axiom,
    ( accept_number(countryahumanitarianorganization,n1) = true )).

cnf(deduced_244,axiom,
    ( accept_population(countryahumanitarianorganization,other,n0) = true )).

cnf(deduced_245,axiom,
    ( accept_population(countryahumanitarianorganization,native,n0) = true )).

cnf(deduced_246,axiom,
    ( accept_population(countryahumanitarianorganization,muslim,n1) = true )).

cnf(deduced_247,axiom,
    ( accept_population(countryahumanitarianorganization,christian,n24) = true )).

cnf(deduced_248,axiom,
    ( accept_population(countryahumanitarianorganization,atheist,n75) = true )).

cnf(deduced_249,axiom,
    ( accept_number(countryahumanitarianorganization,n6) = true )).

cnf(deduced_250,axiom,
    ( accept_leader(countryahumanitarianorganization,sufferterragovernment) = true )).

cnf(deduced_251,axiom,
    ( accept_city(countryahumanitarianorganization,towna) = true )).

cnf(deduced_252,axiom,
    ( accept_leader(sufferterragovernment,countryacivilorganization) = true )).

cnf(deduced_253,axiom,
    ( accept_population(sufferterragovernment,other,n1) = true )).

cnf(deduced_254,axiom,
    ( accept_population(sufferterragovernment,native,n0) = true )).

cnf(deduced_255,axiom,
    ( accept_population(sufferterragovernment,muslim,n1) = true )).

cnf(deduced_256,axiom,
    ( accept_population(sufferterragovernment,christian,n20) = true )).

cnf(deduced_257,axiom,
    ( accept_population(sufferterragovernment,atheist,n78) = true )).

cnf(deduced_258,axiom,
    ( accept_city(sufferterragovernment,cityb) = true )).

cnf(deduced_259,axiom,
    ( accept_leader(sufferterragovernment,countrybhumanitarianorganization) = true )).

cnf(deduced_260,axiom,
    ( accept_population(countryccivilorganization,other,n0) = true )).

cnf(deduced_261,axiom,
    ( accept_population(countryccivilorganization,native,n0) = true )).

cnf(deduced_262,axiom,
    ( accept_population(countryccivilorganization,muslim,n1) = true )).

cnf(deduced_263,axiom,
    ( accept_population(countryccivilorganization,christian,n24) = true )).

cnf(deduced_264,axiom,
    ( accept_population(countryccivilorganization,atheist,n75) = true )).

cnf(deduced_265,axiom,
    ( accept_city(countryccivilorganization,towna) = true )).

cnf(deduced_266,axiom,
    ( accept_leader(countryccivilorganization,sufferterragovernment) = true )).

cnf(deduced_267,axiom,
    ( accept_number(countryafirstaidorganization,n5) = true )).

cnf(deduced_268,axiom,
    ( accept_number(countryafirstaidorganization,n4) = true )).

cnf(deduced_269,axiom,
    ( accept_number(countryafirstaidorganization,n3) = true )).

cnf(deduced_270,axiom,
    ( accept_number(countryafirstaidorganization,n2) = true )).

cnf(deduced_271,axiom,
    ( accept_number(countryafirstaidorganization,n1) = true )).

cnf(deduced_272,axiom,
    ( accept_population(countryafirstaidorganization,other,n0) = true )).

cnf(deduced_273,axiom,
    ( accept_population(countryafirstaidorganization,native,n0) = true )).

cnf(deduced_274,axiom,
    ( accept_population(countryafirstaidorganization,muslim,n1) = true )).

cnf(deduced_275,axiom,
    ( accept_population(countryafirstaidorganization,christian,n24) = true )).

cnf(deduced_276,axiom,
    ( accept_population(countryafirstaidorganization,atheist,n75) = true )).

cnf(deduced_277,axiom,
    ( accept_number(countryafirstaidorganization,n6) = true )).

cnf(deduced_278,axiom,
    ( accept_leader(countryafirstaidorganization,sufferterragovernment) = true )).

cnf(deduced_279,axiom,
    ( accept_city(countryafirstaidorganization,towna) = true )).

cnf(deduced_280,axiom,
    ( accept_leader(countrycmedicalorganization,countryccivilorganization) = true )).

cnf(deduced_281,axiom,
    ( accept_leader(countrycmedicalorganization,countrybcivilorganization) = true )).

cnf(deduced_282,axiom,
    ( accept_leader(countrycmedicalorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_283,axiom,
    ( accept_leader(countrycmedicalorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_284,axiom,
    ( accept_population(countrycmedicalorganization,other,n0) = true )).

cnf(deduced_285,axiom,
    ( accept_population(countrycmedicalorganization,native,n0) = true )).

cnf(deduced_286,axiom,
    ( accept_population(countrycmedicalorganization,muslim,n1) = true )).

cnf(deduced_287,axiom,
    ( accept_population(countrycmedicalorganization,christian,n24) = true )).

cnf(deduced_288,axiom,
    ( accept_population(countrycmedicalorganization,atheist,n75) = true )).

cnf(deduced_289,axiom,
    ( accept_city(countrycmedicalorganization,towna) = true )).

cnf(deduced_290,axiom,
    ( accept_leader(countrycmedicalorganization,sufferterragovernment) = true )).

cnf(deduced_291,axiom,
    ( accept_leader(countrybcivilorganization,countryccivilorganization) = true )).

cnf(deduced_292,axiom,
    ( accept_leader(countrybcivilorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_293,axiom,
    ( accept_leader(countrybcivilorganization,christiancountrychumanitarianorganization) = true )).

cnf(deduced_294,axiom,
    ( accept_leader(countrybcivilorganization,countrycmedicalorganization) = true )).

cnf(deduced_295,axiom,
    ( accept_population(countrybcivilorganization,other,n0) = true )).

cnf(deduced_296,axiom,
    ( accept_population(countrybcivilorganization,native,n0) = true )).

cnf(deduced_297,axiom,
    ( accept_population(countrybcivilorganization,muslim,n1) = true )).

cnf(deduced_298,axiom,
    ( accept_population(countrybcivilorganization,christian,n24) = true )).

cnf(deduced_299,axiom,
    ( accept_population(countrybcivilorganization,atheist,n75) = true )).

cnf(deduced_300,axiom,
    ( accept_city(countrybcivilorganization,towna) = true )).

cnf(deduced_301,axiom,
    ( accept_leader(countrybcivilorganization,sufferterragovernment) = true )).

cnf(deduced_302,axiom,
    ( accept_number(sufferterragovernment,n5) = true )).

cnf(deduced_303,axiom,
    ( accept_number(sufferterragovernment,n4) = true )).

cnf(deduced_304,axiom,
    ( accept_number(sufferterragovernment,n3) = true )).

cnf(deduced_305,axiom,
    ( accept_number(sufferterragovernment,n2) = true )).

cnf(deduced_306,axiom,
    ( accept_number(sufferterragovernment,n1) = true )).

cnf(deduced_307,axiom,
    ( accept_number(sufferterragovernment,n6) = true )).

cnf(deduced_308,axiom,
    ( accept_number(countryacivilorganization,n5) = true )).

cnf(deduced_309,axiom,
    ( accept_number(countryacivilorganization,n4) = true )).

cnf(deduced_310,axiom,
    ( accept_number(countryacivilorganization,n3) = true )).

cnf(deduced_311,axiom,
    ( accept_number(countryacivilorganization,n2) = true )).

cnf(deduced_312,axiom,
    ( accept_number(countryacivilorganization,n1) = true )).

cnf(deduced_313,axiom,
    ( accept_population(countryacivilorganization,other,n0) = true )).

cnf(deduced_314,axiom,
    ( accept_population(countryacivilorganization,native,n0) = true )).

cnf(deduced_315,axiom,
    ( accept_population(countryacivilorganization,muslim,n1) = true )).

cnf(deduced_316,axiom,
    ( accept_population(countryacivilorganization,christian,n24) = true )).

cnf(deduced_317,axiom,
    ( accept_population(countryacivilorganization,atheist,n75) = true )).

cnf(deduced_318,axiom,
    ( accept_number(countryacivilorganization,n6) = true )).

cnf(deduced_319,axiom,
    ( accept_leader(countryacivilorganization,sufferterragovernment) = true )).

cnf(deduced_320,axiom,
    ( accept_city(countryacivilorganization,towna) = true )).

cnf(deduced_321,axiom,
    ( accept_number(countryccivilorganization,n3) = true )).

cnf(deduced_322,axiom,
    ( accept_number(countryccivilorganization,n2) = true )).

cnf(deduced_323,axiom,
    ( accept_number(countryccivilorganization,n1) = true )).

cnf(deduced_324,axiom,
    ( accept_number(countryccivilorganization,n4) = true )).

cnf(deduced_325,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,countryccivilorganization) = true )).

cnf(deduced_326,axiom,
    ( accept_number(countrybcivilorganization,n5) = true )).

cnf(deduced_327,axiom,
    ( accept_number(countrybcivilorganization,n4) = true )).

cnf(deduced_328,axiom,
    ( accept_number(countrybcivilorganization,n3) = true )).

cnf(deduced_329,axiom,
    ( accept_number(countrybcivilorganization,n2) = true )).

cnf(deduced_330,axiom,
    ( accept_number(countrybcivilorganization,n1) = true )).

cnf(deduced_331,axiom,
    ( accept_number(countrybcivilorganization,n6) = true )).

cnf(deduced_332,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,countrybcivilorganization) = true )).

cnf(deduced_333,axiom,
    ( accept_number(countrybhumanitarianorganization,n5) = true )).

cnf(deduced_334,axiom,
    ( accept_number(countrybhumanitarianorganization,n4) = true )).

cnf(deduced_335,axiom,
    ( accept_number(countrybhumanitarianorganization,n3) = true )).

cnf(deduced_336,axiom,
    ( accept_number(countrybhumanitarianorganization,n2) = true )).

cnf(deduced_337,axiom,
    ( accept_number(countrybhumanitarianorganization,n1) = true )).

cnf(deduced_338,axiom,
    ( accept_number(countrybhumanitarianorganization,n6) = true )).

cnf(deduced_339,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,countrybhumanitarianorganization) = true )).

cnf(deduced_340,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,countrycmedicalorganization) = true )).

cnf(deduced_341,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n5) = true )).

cnf(deduced_342,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n4) = true )).

cnf(deduced_343,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n3) = true )).

cnf(deduced_344,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n2) = true )).

cnf(deduced_345,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n1) = true )).

cnf(deduced_346,axiom,
    ( accept_number(christiancountrychumanitarianorganization,n6) = true )).

cnf(deduced_347,axiom,
    ( accept_population(christiancountrychumanitarianorganization,other,n0) = true )).

cnf(deduced_348,axiom,
    ( accept_population(christiancountrychumanitarianorganization,native,n0) = true )).

cnf(deduced_349,axiom,
    ( accept_population(christiancountrychumanitarianorganization,muslim,n1) = true )).

cnf(deduced_350,axiom,
    ( accept_population(christiancountrychumanitarianorganization,christian,n24) = true )).

cnf(deduced_351,axiom,
    ( accept_population(christiancountrychumanitarianorganization,atheist,n75) = true )).

cnf(deduced_352,axiom,
    ( accept_city(christiancountrychumanitarianorganization,towna) = true )).

cnf(deduced_353,axiom,
    ( accept_leader(christiancountrychumanitarianorganization,sufferterragovernment) = true )).

cnf(deduced_354,axiom,
    ( accept_number(countrycmedicalorganization,n5) = true )).

cnf(deduced_355,axiom,
    ( accept_number(countrycmedicalorganization,n4) = true )).

cnf(deduced_356,axiom,
    ( accept_number(countrycmedicalorganization,n3) = true )).

cnf(deduced_357,axiom,
    ( accept_number(countrycmedicalorganization,n2) = true )).

cnf(deduced_358,axiom,
    ( accept_number(countrycmedicalorganization,n1) = true )).

cnf(deduced_359,axiom,
    ( accept_number(countrycmedicalorganization,n6) = true )).

cnf(deduced_360,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n5) = true )).

cnf(deduced_361,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n4) = true )).

cnf(deduced_362,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n3) = true )).

cnf(deduced_363,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n2) = true )).

cnf(deduced_364,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n1) = true )).

cnf(deduced_365,axiom,
    ( accept_number(muslimcountrybhumanitarianorganization,n6) = true )).

cnf(deduced_367,axiom,
    ( accept_leader(muslimcountrybhumanitarianorganization,sufferterragovernment) = true )).

cnf(a1_6,axiom,
    ( ifeq(tuple(accept_city(A,C),accept_leader(A,L),the_agent_not_in_any_proposed_teams(A,L,C)),tuple(true,true,true),a,b) = b )).

cnf(event_9,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrycmedicalorganization,towna,n4),true,a,b) = b )).

cnf(event_10,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrycmedicalorganization,towna,n5),true,a,b) = b )).

cnf(event_29,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n2),true,a,b) = b )).

cnf(event_30,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,countryafirstaidorganization,coastvillage,n2),true,a,b) = b )).

cnf(event_31,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,countryacivilorganization,coastvillage,n2),true,a,b) = b )).

cnf(event_32,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,christiansufferterrahumanitarianorganization,coastvillage,n2),true,a,b) = b )).

cnf(event_33,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,sufferterragovernment,coastvillage,n2),true,a,b) = b )).

cnf(event_34,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,countryahumanitarianorganization,coastvillage,n6),true,a,b) = b )).

cnf(event_35,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,countryacivilorganization,coastvillage,n6),true,a,b) = b )).

cnf(event_36,axiom,
    ( ifeq2(accept_team(countryamedicalorganization,countryafirstaidorganization,coastvillage,n6),true,a,b) = b )).

cnf(event_40,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrycmedicalorganization,towna,n5),true,a,b) = b )).

cnf(event_71,axiom,
    ( ifeq2(accept_team(countryccivilorganization,christiancountrychumanitarianorganization,coastvillage,n5),true,a,b) = b )).

cnf(event_72,axiom,
    ( ifeq2(accept_team(countryccivilorganization,christiancountrychumanitarianorganization,coastvillage,n6),true,a,b) = b )).

cnf(event_115,axiom,
    ( ifeq2(accept_team(countryccivilorganization,muslimcountrybhumanitarianorganization,townc,n6),true,a,b) = b )).

cnf(event_116,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrybcivilorganization,townc,n6),true,a,b) = b )).

cnf(event_117,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrybhumanitarianorganization,townc,n6),true,a,b) = b )).

cnf(event_118,axiom,
    ( ifeq2(accept_team(countryccivilorganization,christiancountrychumanitarianorganization,townc,n6),true,a,b) = b )).

cnf(event_119,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrycmedicalorganization,townc,n6),true,a,b) = b )).

cnf(event_143,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrybhumanitarianorganization,cityb,n5),true,a,b) = b )).

cnf(event_144,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrybhumanitarianorganization,cityb,n6),true,a,b) = b )).

cnf(event_174,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrybcivilorganization,towna,n2),true,a,b) = b )).

cnf(event_175,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrycmedicalorganization,towna,n2),true,a,b) = b )).

cnf(event_176,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization,towna,n2),true,a,b) = b )).

cnf(event_177,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countryccivilorganization,towna,n2),true,a,b) = b )).

cnf(event_178,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrybcivilorganization,towna,n3),true,a,b) = b )).

cnf(event_179,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization,towna,n3),true,a,b) = b )).

cnf(event_180,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrycmedicalorganization,towna,n3),true,a,b) = b )).

cnf(event_181,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countryccivilorganization,towna,n3),true,a,b) = b )).

cnf(event_182,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization,towna,n2),true,a,b) = b )).

cnf(event_184,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countryccivilorganization,towna,n4),true,a,b) = b )).

cnf(event_185,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrybcivilorganization,towna,n6),true,a,b) = b )).

cnf(event_186,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrybhumanitarianorganization,towna,n6),true,a,b) = b )).

cnf(event_187,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,christiancountrychumanitarianorganization,towna,n6),true,a,b) = b )).

cnf(event_212,axiom,
    ( ifeq2(accept_team(christiansufferterrahumanitarianorganization,sufferterragovernment,towna,n2),true,a,b) = b )).

cnf(event_213,axiom,
    ( ifeq2(accept_team(christiansufferterrahumanitarianorganization,sufferterragovernment,towna,n6),true,a,b) = b )).

cnf(event_216,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrybcivilorganization,towna,n6),true,a,b) = b )).

cnf(event_217,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrybhumanitarianorganization,towna,n6),true,a,b) = b )).

cnf(event_218,axiom,
    ( ifeq2(accept_team(countryccivilorganization,christiancountrychumanitarianorganization,towna,n6),true,a,b) = b )).

cnf(event_219,axiom,
    ( ifeq2(accept_team(countryccivilorganization,countrycmedicalorganization,towna,n6),true,a,b) = b )).

cnf(event_246,axiom,
    ( ifeq2(accept_team(muslimcountrybhumanitarianorganization,countrycmedicalorganization,towna,n6),true,a,b) = b )).

cnf(deduced_13,axiom,
    ( ifeq2(accept_city(countryamedicalorganization,coastvillage),true,a,b) = b )).

cnf(deduced_58,axiom,
    ( ifeq2(accept_number(countryccivilorganization,n5),true,a,b) = b )).

cnf(deduced_62,axiom,
    ( ifeq2(accept_number(countryccivilorganization,n6),true,a,b) = b )).

cnf(deduced_82,axiom,
    ( ifeq2(accept_leader(christiansufferterrahumanitarianorganization,sufferterragovernment),true,a,b) = b )).

cnf(deduced_366,axiom,
    ( ifeq2(accept_city(muslimcountrybhumanitarianorganization,towna),true,a,b) = b )).

cnf(goal,negated_conjecture,
    ( a != b )).

%------------------------------------------------------------------------------