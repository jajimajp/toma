%------------------------------------------------------------------------------
% File     : SWB004-10 : TPTP v7.5.0. Released v7.3.0.
% Domain   : Puzzles
% Problem  : Axiomatic Triples
% Version  : Especial.
% English  :

% Refs     : [CS18]  Claessen & Smallbone (2018), Efficient Encodings of Fi
%          : [Sma18] Smallbone (2018), Email to Geoff Sutcliffe
% Source   : [Sma18]
% Names    :

% Status   : Satisfiable
% Rating   : 0.00 v7.3.0
% Syntax   : Number of clauses     :   91 (   0 non-Horn;  91 unit;  62 RR)
%            Number of atoms       :   91 (  91 equality)
%            Maximal clause size   :    1 (   1 average)
%            Number of predicates  :    1 (   0 propositional; 2-2 arity)
%            Number of functors    :   45 (  37 constant; 0-5 arity)
%            Number of variables   :   54 (  10 singleton)
%            Maximal term depth    :    4 (   2 average)
% SPC      : CNF_SAT_RFO_PEQ_UEQ

% Comments : Converted from SWB004+4 to UEQ using [CS18].
%------------------------------------------------------------------------------
cnf(ifeq_axiom,axiom,
    ( ifeq(A,A,B,C) = B )).

cnf(simple_iext_property,axiom,
    ( ifeq(iext(P,S,O),true,ip(P),true) = true )).

cnf(simple_ir,axiom,
    ( ir(X) = true )).

cnf(simple_lv,axiom,
    ( ifeq(lv(X),true,ir(X),true) = true )).

cnf(rdf_collection_first_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_first,uri_rdf_Property) = true )).

cnf(rdf_collection_nil_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_nil,uri_rdf_List) = true )).

cnf(rdf_collection_rest_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_rest,uri_rdf_Property) = true )).

cnf(rdf_container_n_type_001,axiom,
    ( iext(uri_rdf_type,uri_rdf__1,uri_rdf_Property) = true )).

cnf(rdf_container_n_type_002,axiom,
    ( iext(uri_rdf_type,uri_rdf__2,uri_rdf_Property) = true )).

cnf(rdf_container_n_type_003,axiom,
    ( iext(uri_rdf_type,uri_rdf__3,uri_rdf_Property) = true )).

cnf(rdf_reification_object_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_object,uri_rdf_Property) = true )).

cnf(rdf_reification_predicate_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_value,uri_rdf_Property) = true )).

cnf(rdf_reification_subject_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_subject,uri_rdf_Property) = true )).

cnf(rdf_type_ip_1,axiom,
    ( ifeq(ip(P),true,iext(uri_rdf_type,P,uri_rdf_Property),true) = true )).

cnf(rdf_type_ip,axiom,
    ( ifeq(iext(uri_rdf_type,P,uri_rdf_Property),true,ip(P),true) = true )).

cnf(rdf_type_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_type,uri_rdf_Property) = true )).

cnf(rdf_value_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_type,uri_rdf_Property) = true )).

cnf(rdfs_annotation_comment_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_comment,uri_rdfs_Resource) = true )).

cnf(rdfs_annotation_comment_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_comment,uri_rdfs_Literal) = true )).

cnf(rdfs_annotation_isdefinedby_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_isDefinedBy,uri_rdfs_Resource) = true )).

cnf(rdfs_annotation_isdefinedby_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_isDefinedBy,uri_rdfs_Resource) = true )).

cnf(rdfs_annotation_isdefinedby_sub,axiom,
    ( iext(uri_rdfs_subPropertyOf,uri_rdfs_isDefinedBy,uri_rdfs_seeAlso) = true )).

cnf(rdfs_annotation_label_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_label,uri_rdfs_Resource) = true )).

cnf(rdfs_annotation_label_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_label,uri_rdfs_Literal) = true )).

cnf(rdfs_annotation_seealso_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_seeAlso,uri_rdfs_Resource) = true )).

cnf(rdfs_annotation_seealso_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_seeAlso,uri_rdfs_Resource) = true )).

cnf(rdfs_cext_def_1,axiom,
    ( ifeq(icext(C,X),true,iext(uri_rdf_type,X,C),true) = true )).

cnf(rdfs_cext_def,axiom,
    ( ifeq(iext(uri_rdf_type,X,C),true,icext(C,X),true) = true )).

cnf(rdfs_class_instsub_resource,axiom,
    ( ifeq(ic(C),true,iext(uri_rdfs_subClassOf,C,uri_rdfs_Resource),true) = true )).

cnf(rdfs_collection_first_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_first,uri_rdf_List) = true )).

cnf(rdfs_collection_first_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_first,uri_rdfs_Resource) = true )).

cnf(rdfs_collection_rest_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_rest,uri_rdf_List) = true )).

cnf(rdfs_collection_rest_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_rest,uri_rdf_List) = true )).

cnf(rdfs_container_alt_sub,axiom,
    ( iext(uri_rdfs_subClassOf,uri_rdf_Alt,uri_rdfs_Container) = true )).

cnf(rdfs_container_bag_sub,axiom,
    ( iext(uri_rdfs_subClassOf,uri_rdf_Bag,uri_rdfs_Container) = true )).

cnf(rdfs_container_containermembershipproperty_instsub_member,axiom,
    ( ifeq(icext(uri_rdfs_ContainerMembershipProperty,P),true,iext(uri_rdfs_subPropertyOf,P,uri_rdfs_member),true) = true )).

cnf(rdfs_container_containermembershipproperty_sub,axiom,
    ( iext(uri_rdfs_subClassOf,uri_rdfs_ContainerMembershipProperty,uri_rdf_Property) = true )).

cnf(rdfs_container_member_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_member,uri_rdfs_Resource) = true )).

cnf(rdfs_container_member_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_member,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_domain_001,axiom,
    ( iext(uri_rdfs_domain,uri_rdf__1,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_domain_002,axiom,
    ( iext(uri_rdfs_domain,uri_rdf__2,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_domain_003,axiom,
    ( iext(uri_rdfs_domain,uri_rdf__3,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_range_001,axiom,
    ( iext(uri_rdfs_range,uri_rdf__1,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_range_002,axiom,
    ( iext(uri_rdfs_range,uri_rdf__2,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_range_003,axiom,
    ( iext(uri_rdfs_range,uri_rdf__3,uri_rdfs_Resource) = true )).

cnf(rdfs_container_n_type_001,axiom,
    ( iext(uri_rdf_type,uri_rdf__1,uri_rdfs_ContainerMembershipProperty) = true )).

cnf(rdfs_container_n_type_002,axiom,
    ( iext(uri_rdf_type,uri_rdf__2,uri_rdfs_ContainerMembershipProperty) = true )).

cnf(rdfs_container_n_type_003,axiom,
    ( iext(uri_rdf_type,uri_rdf__3,uri_rdfs_ContainerMembershipProperty) = true )).

cnf(rdfs_container_seq_sub,axiom,
    ( iext(uri_rdfs_subClassOf,uri_rdfs_Seq,uri_rdfs_Container) = true )).

cnf(rdfs_dat_xmlliteral_sub,axiom,
    ( iext(uri_rdfs_subClassOf,uri_rdf_XMLLiteral,uri_rdfs_Literal) = true )).

cnf(rdfs_dat_xmlliteral_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_XMLLiteral,uri_rdfs_Datatype) = true )).

cnf(rdfs_datatype_instsub_literal,axiom,
    ( ifeq(icext(uri_rdfs_Datatype,D),true,iext(uri_rdfs_subClassOf,D,uri_rdfs_Literal),true) = true )).

cnf(rdfs_datatype_sub,axiom,
    ( iext(uri_rdfs_subClassOf,uri_rdfs_Datatype,uri_rdfs_Class) = true )).

cnf(rdfs_domain_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_domain,uri_rdf_Property) = true )).

cnf(rdfs_domain_main,axiom,
    ( ifeq(iext(uri_rdfs_domain,P,C),true,ifeq(iext(P,X,Y),true,icext(C,X),true),true) = true )).

cnf(rdfs_domain_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_domain,uri_rdfs_Class) = true )).

cnf(rdfs_ic_def_1,axiom,
    ( ifeq(ic(X),true,icext(uri_rdfs_Class,X),true) = true )).

cnf(rdfs_ic_def,axiom,
    ( ifeq(icext(uri_rdfs_Class,X),true,ic(X),true) = true )).

cnf(rdfs_ir_def_1,axiom,
    ( ifeq(icext(uri_rdfs_Resource,X),true,ir(X),true) = true )).

cnf(rdfs_ir_def,axiom,
    ( ifeq(ir(X),true,icext(uri_rdfs_Resource,X),true) = true )).

cnf(rdfs_lv_def_1,axiom,
    ( ifeq(icext(uri_rdfs_Literal,X),true,lv(X),true) = true )).

cnf(rdfs_lv_def,axiom,
    ( ifeq(lv(X),true,icext(uri_rdfs_Literal,X),true) = true )).

cnf(rdfs_property_type,axiom,
    ( iext(uri_rdf_type,uri_rdf_Property,uri_rdfs_Class) = true )).

cnf(rdfs_range_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_range,uri_rdf_Property) = true )).

cnf(rdfs_range_main,axiom,
    ( ifeq(iext(uri_rdfs_range,P,C),true,ifeq(iext(P,X,Y),true,icext(C,Y),true),true) = true )).

cnf(rdfs_range_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_range,uri_rdfs_Class) = true )).

cnf(rdfs_reification_object_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_object,uri_rdfs_Statement) = true )).

cnf(rdfs_reification_object_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_predicate,uri_rdfs_Resource) = true )).

cnf(rdfs_reification_predicate_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_predicate,uri_rdfs_Statement) = true )).

cnf(rdfs_reification_predicate_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_predicate,uri_rdfs_Resource) = true )).

cnf(rdfs_reification_subject_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_subject,uri_rdfs_Statement) = true )).

cnf(rdfs_reification_subject_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_subject,uri_rdfs_Resource) = true )).

cnf(rdfs_subclassof_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_subClassOf,uri_rdfs_Class) = true )).

cnf(rdfs_subclassof_main_2,axiom,
    ( ifeq(icext(C,X),true,ifeq(iext(uri_rdfs_subClassOf,C,D),true,icext(D,X),true),true) = true )).

cnf(rdfs_subclassof_main_1,axiom,
    ( ifeq(iext(uri_rdfs_subClassOf,C,D),true,ic(D),true) = true )).

cnf(rdfs_subclassof_main,axiom,
    ( ifeq(iext(uri_rdfs_subClassOf,C,D),true,ic(C),true) = true )).

cnf(rdfs_subclassof_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_subClassOf,uri_rdfs_Class) = true )).

cnf(rdfs_subclassof_reflex,axiom,
    ( ifeq(ic(C),true,iext(uri_rdfs_subClassOf,C,C),true) = true )).

cnf(rdfs_subclassof_trans,axiom,
    ( ifeq(iext(uri_rdfs_subClassOf,D,E),true,ifeq(iext(uri_rdfs_subClassOf,C,D),true,iext(uri_rdfs_subClassOf,C,E),true),true) = true )).

cnf(rdfs_subpropertyof_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdfs_subPropertyOf,uri_rdf_Property) = true )).

cnf(rdfs_subpropertyof_main_2,axiom,
    ( ifeq(iext(uri_rdfs_subPropertyOf,P,Q),true,ip(Q),true) = true )).

cnf(rdfs_subpropertyof_main_1,axiom,
    ( ifeq(iext(uri_rdfs_subPropertyOf,P,Q),true,ip(P),true) = true )).

cnf(rdfs_subpropertyof_main,axiom,
    ( ifeq(iext(uri_rdfs_subPropertyOf,P,Q),true,ifeq(iext(P,X,Y),true,iext(Q,X,Y),true),true) = true )).

cnf(rdfs_subpropertyof_range,axiom,
    ( iext(uri_rdfs_range,uri_rdfs_subPropertyOf,uri_rdf_Property) = true )).

cnf(rdfs_subpropertyof_reflex,axiom,
    ( ifeq(ip(P),true,iext(uri_rdfs_subPropertyOf,P,P),true) = true )).

cnf(rdfs_subpropertyof_trans,axiom,
    ( ifeq(iext(uri_rdfs_subPropertyOf,Q,R),true,ifeq(iext(uri_rdfs_subPropertyOf,P,Q),true,iext(uri_rdfs_subPropertyOf,P,R),true),true) = true )).

cnf(rdfs_type_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_type,uri_rdfs_Resource) = true )).

cnf(rdfs_type_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_type,uri_rdfs_Class) = true )).

cnf(rdfs_value_domain,axiom,
    ( iext(uri_rdfs_domain,uri_rdf_value,uri_rdfs_Resource) = true )).

cnf(rdfs_value_range,axiom,
    ( iext(uri_rdfs_range,uri_rdf_value,uri_rdfs_Resource) = true )).

cnf(testcase_conclusion_fullish_004_Axiomatic_Triples,negated_conjecture,
    ( tuple(iext(uri_rdfs_subClassOf,uri_owl_Class,uri_owl_Thing),iext(uri_rdfs_subClassOf,uri_rdfs_Datatype,uri_owl_Class),iext(uri_rdf_type,uri_owl_Class,uri_owl_Class),iext(uri_rdf_type,uri_owl_Class,uri_owl_Thing),iext(uri_owl_equivalentClass,uri_owl_Class,uri_rdfs_Class)) != tuple(true,true,true,true,true) )).

%------------------------------------------------------------------------------