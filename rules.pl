% ====================================================
%  Inference rules (derive/3)
% ====================================================

% --- NAL-1 ---
% Revision
derive(sentence(T,T1), sentence(T,T2), sentence(T,R)) :-
    truth_revision(T1,T2,R).

% Inheritance syllogisms
derive(sentence(inheritance(A,B),T1),
       sentence(inheritance(B,C),T2),
       sentence(inheritance(A,C),R)) :-
    truth_deduction(T1,T2,R).

derive(sentence(inheritance(A,B),T1),
       sentence(inheritance(A,C),T2),
       sentence(inheritance(C,B),R)) :-
    truth_induction(T1,T2,R).

derive(sentence(inheritance(A,C),T1),
       sentence(inheritance(B,C),T2),
       sentence(inheritance(B,A),R)) :-
    truth_abduction(T1,T2,R).

derive(sentence(inheritance(A,B),T1),
       sentence(inheritance(B,C),T2),
       sentence(inheritance(C,A),R)) :-
    truth_exemplification(T1,T2,R).

% --- NAL-2 ---
derive(sentence(similarity(S,P),T), none,
       sentence(similarity(P,S),R)) :-
    truth_structural_intersection(T,R).

derive(sentence(similarity(M,P),T1),
       sentence(similarity(S,M),T2),
       sentence(similarity(S,P),R)) :-
    truth_resemblance(T1,T2,R).

derive(sentence(inheritance(P,M),T1),
       sentence(inheritance(S,M),T2),
       sentence(similarity(S,P),R)) :-
    truth_comparison(T1,T2,R).

derive(sentence(inheritance(M,P),T1),
       sentence(inheritance(M,S),T2),
       sentence(similarity(S,P),R)) :-
    truth_comparison(T1,T2,R).

derive(sentence(inheritance(M,P),T1),
       sentence(similarity(S,M),T2),
       sentence(inheritance(S,P),R)) :-
    truth_analogy(T1,T2,R).

derive(sentence(inheritance(P,M),T1),
       sentence(similarity(S,M),T2),
       sentence(inheritance(P,S),R)) :-
    truth_analogy(T1,T2,R).





% =========================
% NAL-3
% =========================

% --- Set decomposition ---
derive(sentence(inheritance(extset(A,B), M), T),
       none,
       sentence(inheritance(extset(A), M), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(extset(A,B), M), T),
       none,
       sentence(inheritance(extset(B), M), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(M, intset(A,B)), T),
       none,
       sentence(inheritance(M, intset(A)), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(M, intset(A,B)), T),
       none,
       sentence(inheritance(M, intset(B)), R)) :-
    truth_structural_deduction(T, R).

% --- Extensional/intensional intersection decomposition ---
derive(sentence(inheritance(intint(S,P), M), T),
       none,
       sentence(inheritance(S, M), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(intint(S,P), M), T),
       none,
       sentence(inheritance(P, M), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(M, extint(S,P)), T),
       none,
       sentence(inheritance(M, S), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(M, extint(S,P)), T),
       none,
       sentence(inheritance(M, P), R)) :-
    truth_structural_deduction(T, R).

% --- Diff decomposition (and negated structural) ---
derive(sentence(inheritance(intdiff(A,S), M), T),
       none,
       sentence(inheritance(A, M), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(M, extdiff(B,S)), T),
       none,
       sentence(inheritance(M, B), R)) :-
    truth_structural_deduction(T, R).

derive(sentence(inheritance(intdiff(A,S), M), T),
       none,
       sentence(inheritance(S, M), R)) :-
    truth_structural_deduction_neg(T, R).

derive(sentence(inheritance(M, extdiff(B,S)), T),
       none,
       sentence(inheritance(M, S), R)) :-
    truth_structural_deduction_neg(T, R).

% --- Intersection/diff decomposition via PNN/NPP/PNP/NNN (subject side) ---
derive(sentence(inheritance(S, M), T1),
       sentence(inheritance(intint(S,P), M), T2),
       sentence(inheritance(P, M), R)) :-
    truth_decompose_pnn(T1, T2, R).

derive(sentence(inheritance(P, M), T1),
       sentence(inheritance(intint(S,P), M), T2),
       sentence(inheritance(S, M), R)) :-
    truth_decompose_pnn(T1, T2, R).

derive(sentence(inheritance(S, M), T1),
       sentence(inheritance(extint(S,P), M), T2),
       sentence(inheritance(P, M), R)) :-
    truth_decompose_npp(T1, T2, R).

derive(sentence(inheritance(P, M), T1),
       sentence(inheritance(extint(S,P), M), T2),
       sentence(inheritance(S, M), R)) :-
    truth_decompose_npp(T1, T2, R).

derive(sentence(inheritance(S, M), T1),
       sentence(inheritance(intdiff(S,P), M), T2),
       sentence(inheritance(P, M), R)) :-
    truth_decompose_pnp(T1, T2, R).

derive(sentence(inheritance(S, M), T1),
       sentence(inheritance(intdiff(P,S), M), T2),
       sentence(inheritance(P, M), R)) :-
    truth_decompose_nnn(T1, T2, R).

% --- Intersection/diff decomposition via PNN/NPP/PNP/NNN (object side) ---
derive(sentence(inheritance(M, S), T1),
       sentence(inheritance(M, extint(S,P)), T2),
       sentence(inheritance(M, P), R)) :-
    truth_decompose_pnn(T1, T2, R).

derive(sentence(inheritance(M, P), T1),
       sentence(inheritance(M, extint(S,P)), T2),
       sentence(inheritance(M, S), R)) :-
    truth_decompose_pnn(T1, T2, R).

derive(sentence(inheritance(M, S), T1),
       sentence(inheritance(M, intint(S,P)), T2),
       sentence(inheritance(M, P), R)) :-
    truth_decompose_npp(T1, T2, R).

derive(sentence(inheritance(M, P), T1),
       sentence(inheritance(M, intint(S,P)), T2),
       sentence(inheritance(M, S), R)) :-
    truth_decompose_npp(T1, T2, R).

derive(sentence(inheritance(M, S), T1),
       sentence(inheritance(M, extdiff(S,P)), T2),
       sentence(inheritance(M, P), R)) :-
    truth_decompose_pnp(T1, T2, R).

derive(sentence(inheritance(M, S), T1),
       sentence(inheritance(M, extdiff(P,S)), T2),
       sentence(inheritance(M, P), R)) :-
    truth_decompose_nnn(T1, T2, R).

% =========================
% NAL-4 (optional relation-component reasoning)
% =========================

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(product(C,B), R), T2),
       sentence(inheritance(C, A), ROut)) :-
    truth_abduction(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(product(A,C), R), T2),
       sentence(inheritance(C, B), ROut)) :-
    truth_abduction(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(R, product(C,B)), T2),
       sentence(inheritance(C, A), ROut)) :-
    truth_induction(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(R, product(A,C)), T2),
       sentence(inheritance(C, B), ROut)) :-
    truth_induction(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(C, A), T2),
       sentence(inheritance(product(C,B), R), ROut)) :-
    truth_deduction(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(A, C), T2),
       sentence(inheritance(product(C,B), R), ROut)) :-
    truth_induction(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(similarity(C, A), T2),
       sentence(inheritance(product(C,B), R), ROut)) :-
    truth_analogy(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(C, B), T2),
       sentence(inheritance(product(A,C), R), ROut)) :-
    truth_deduction(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(B, C), T2),
       sentence(inheritance(product(A,C), R), ROut)) :-
    truth_induction(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(similarity(C, B), T2),
       sentence(inheritance(product(A,C), R), ROut)) :-
    truth_analogy(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(A, C), T2),
       sentence(inheritance(R, product(C,B)), ROut)) :-
    truth_deduction(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(C, A), T2),
       sentence(inheritance(R, product(C,B)), ROut)) :-
    truth_abduction(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(similarity(C, A), T2),
       sentence(inheritance(R, product(C,B)), ROut)) :-
    truth_analogy(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(B, C), T2),
       sentence(inheritance(R, product(A,C)), ROut)) :-
    truth_deduction(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(C, B), T2),
       sentence(inheritance(R, product(A,C)), ROut)) :-
    truth_abduction(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(similarity(C, B), T2),
       sentence(inheritance(R, product(A,C)), ROut)) :-
    truth_analogy(T1, T2, ROut).

% Similarity from product-pattern matches (comparison)
derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(product(C,B), R), T2),
       sentence(similarity(A, C), ROut)) :-
    truth_comparison(T1, T2, ROut).

derive(sentence(inheritance(product(A,B), R), T1),
       sentence(inheritance(product(A,C), R), T2),
       sentence(similarity(B, C), ROut)) :-
    truth_comparison(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(R, product(C,B)), T2),
      sentence(similarity(A, C), ROut)) :-
    truth_comparison(T1, T2, ROut).

derive(sentence(inheritance(R, product(A,B)), T1),
       sentence(inheritance(R, product(A,C)), T2),
       sentence(similarity(B, C), ROut)) :-
    truth_comparison(T1, T2, ROut).

% =========================
% NAL-5
% =========================

% Negation, conjunction, disjunction decomposition
derive(sentence(neg(A), T), none, sentence(A, R)) :-
    truth_negation(T, R).

derive(sentence(conjunction(A,B), T), none, sentence(A, R)) :-
    truth_structural_deduction(T, R).

derive(sentence(conjunction(A,B), T), none, sentence(B, R)) :-
    truth_structural_deduction(T, R).

derive(sentence(conjunction(A,B), T), none, sentence(conjunction(B,A), R)) :-
    truth_structural_intersection(T, R).

derive(sentence(S, T1),
       sentence(conjunction(S,A), T2),
       sentence(A, R)) :-
    truth_decompose_pnn(T1, T2, R).

derive(sentence(S, T1),
       sentence(disjunction(S,A), T2),
       sentence(A, R)) :-
    truth_decompose_npp(T1, T2, R).

derive(sentence(S, T1),
       sentence(conjunction(neg(S),A), T2),
       sentence(A, R)) :-
    truth_decompose_nnn(T1, T2, R).

derive(sentence(S, T1),
       sentence(disjunction(neg(S),A), T2),
       sentence(A, R)) :-
    truth_decompose_ppp(T1, T2, R).

% Implication syllogisms
derive(sentence(implication(A,B), T1),
       sentence(implication(B,C), T2),
       sentence(implication(A,C), R)) :-
    truth_deduction(T1, T2, R).

derive(sentence(implication(A,B), T1),
       sentence(implication(A,C), T2),
       sentence(implication(C,B), R)) :-
    truth_induction(T1, T2, R).

derive(sentence(implication(A,C), T1),
       sentence(implication(B,C), T2),
       sentence(implication(B,A), R)) :-
    truth_abduction(T1, T2, R).

% Higher-order decomposition
%derive(sentence(A, T1),
%       sentence(implication(A,B), T2),
%       sentence(B, R)) :-
%    truth_deduction(T1, T2, R).
% Keep ONLY the stamp of the first premise.
derive(
  sentence(A, T1, EvA),
  sentence(implication(A, B), T2, _EvImp),   % ignore implication's stamp
  sentence(B, R, EvA)
) :-
  truth_deduction(T1, T2, R).

derive(sentence(A, T1),
       sentence(implication(conjunction(A,B), C), T2),
       sentence(implication(B, C), R)) :-
    truth_deduction(T1, T2, R).

derive(sentence(B, T1),
       sentence(implication(A,B), T2),
       sentence(A, R)) :-
    truth_abduction(T1, T2, R).

derive(sentence(A, T1),
       sentence(equivalence(A,B), T2),
       sentence(B, R)) :-
    truth_analogy(T1, T2, R).
