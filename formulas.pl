% ====================================================
%  NARS Inference Rules in SWI-Prolog
%  (direct translation of your MeTTa code)
% ====================================================

% -------- Truth functions --------
truth_c2w(C,W) :- W is C / (1 - C).
truth_w2c(W,C) :- C is W / (W + 1).

truth_deduction(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    F is F1*F2,
    C is F*(C1*C2).

truth_abduction(stv(F1,C1), stv(F2,C2), stv(F2,C)) :-
    W is F1*C1*C2,
    truth_w2c(W,C).

truth_induction(T1,T2,R) :- truth_abduction(T2,T1,R).

truth_exemplification(stv(F1,C1), stv(F2,C2), stv(1.0,C)) :-
    W is F1*F2*C1*C2,
    truth_w2c(W,C).

truth_structural_deduction(T,R) :-
    truth_deduction(T, stv(1.0,0.9), R).

truth_negation(stv(F,C), stv(FN,C)) :- FN is 1-F.

truth_structural_deduction_neg(T,R) :-
    truth_structural_deduction(T,T1),
    truth_negation(T1,R).

truth_intersection(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    F is F1*F2, C is C1*C2.

truth_structural_intersection(T,R) :-
    truth_intersection(T, stv(1.0,0.9), R).

truth_or(A,B,R) :- R is 1 - (1-A)*(1-B).

truth_comparison(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    truth_or(F1,F2,F0),
    (F0=:=0.0 -> F=0.0 ; F is (F1*F2)/F0),
    truth_w2c(F0*(C1*C2),C).

truth_analogy(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    F is F1*F2, C is (C1*C2)*F2.

truth_resemblance(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    truth_or(F1,F2,Fo),
    F is F1*F2, C is (C1*C2)*Fo.

truth_union(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    truth_or(F1,F2,F), C is C1*C2.

truth_difference(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    F is F1*(1-F2), C is C1*C2.

truth_decompose_pnn(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    Fn is F1*(1-F2),
    F is 1-Fn,
    C is Fn*(C1*C2).

truth_decompose_npp(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    F is (1-F1)*F2,
    C is F*(C1*C2).

truth_decompose_pnp(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    F is F1*(1-F2),
    C is F*(C1*C2).

truth_decompose_ppp(V1,V2,R) :-
    truth_negation(V1,V1n),
    truth_decompose_npp(V1n,V2,R).

truth_decompose_nnn(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    Fn is (1-F1)*(1-F2),
    F is 1-Fn,
    C is Fn*(C1*C2).

truth_eternalize(stv(F,C), stv(F,C1)) :- truth_w2c(C,C1).

truth_revision(stv(F1,C1), stv(F2,C2), stv(F,C)) :-
    truth_c2w(C1,W1), truth_c2w(C2,W2),
    W is W1+W2,
    F0 is (W1*F1 + W2*F2)/W,
    C0 is W/(W+1),
    F is min(1.0,F0),
    C1m is max(C0, max(C1,C2)),
    C is min(0.99,C1m).

truth_expectation(stv(F,C),R) :- R is C*(F-0.5)+0.5.
