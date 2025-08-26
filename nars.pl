:- style_check(-singleton).
% =========================
% Tuple helpers used by PQ
% =========================
without_all([], _A, []).
without_all([A|T], A, R) :- !, without_all(T, A, R).
without_all([H|T], A, [H|R]) :- without_all(T, A, R).

% =========================================
% FAST Variant-aware equality & de-duplication
% =========================================

% make a canonical key for variant checking
canonical_key(Term, Key) :-
    copy_term(Term, T1),
    numbervars(T1, 0, _),    % replace vars with $VAR(N) deterministically
    Key = T1.

% pair Key-Term for sorting
keypair(Term, Key-Term) :- canonical_key(Term, Key).

% O(n log n) unique-by-variant
unique_by_variant_fast(L, R) :-
    maplist(keypair, L, Pairs),
    keysort(Pairs, Sorted),              % sort by canonical key
    keep_first_each_key(Sorted, R).

keep_first_each_key([], []).
keep_first_each_key([K-T|Rest], [T|Out]) :-
    drop_same_key(K, Rest, Rest2),
    keep_first_each_key(Rest2, Out).

drop_same_key(_K, [], []).
drop_same_key(K, [K-_|Rest], Rest2) :- !, drop_same_key(K, Rest, Rest2).
drop_same_key(_K, Rest, Rest).  % next key encountered
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
% =========================
% Config (constants)
% =========================
pln_config(max_steps,       100).
pln_config(task_queue_size, 10).
pln_config(belief_queue_size, 100).
% =========================
% Stamps
% =========================
stamp_disjoint(E1, E2) :-
    \+ (member(X, E1), memberchk(X, E2)).

% Concatenate and keep a stable sorted order (adjust if you need custom order)
stamp_concat(Stamp, [], Stamp) :- !.
stamp_concat(Stamp, Addition, R) :-
    append(Stamp, Addition, Both),
    msort(Both, R).

% =========================
% Priority / evaluators
% =========================
% Priority = confidence; unknown/empty gets very low score
priority_rank(sentence(_Term, stv(_F,C), _Ev), C).
priority_rank(_, -9.9999e4).

priority_rank_neg(Item, N) :- priority_rank(Item, C), N is -C.

confidence_rank((stv(_F,C), _Ev), C).
confidence_rank(_, 0).

% =========================
% BestCandidate & LimitSize
% =========================
% best_candidate(+EvalPredName, +CurrentBestOpt, +List, -BestOpt)
best_candidate(_Eval, Best, [], Best).
best_candidate(Eval, none, [H|T], Best) :- !,
    call(Eval, H, _), best_candidate(Eval, some(H), T, Best).
best_candidate(Eval, some(B0), [H|T], Best) :-
    call(Eval, H,  S1),
    call(Eval, B0, S0),
    ( S1 > S0 -> best_candidate(Eval, some(H), T, Best)
    ;            best_candidate(Eval, some(B0), T, Best)
    ).

limit_size(L, Size, L) :- length(L, N), N =< Size, !.
limit_size(L, Size, R) :-
    best_candidate(priority_rank_neg, none, L, some(MinItem)),
    without_all(L, MinItem, L1),
    limit_size(L1, Size, R).

% =========================
% Bridging NAL derive/3 (no stamps) to stamped sentences
% =========================
% infer(+S1,+S2,-Term,-Truth)
% Accepts single-premise rules by passing 'none' as S2.

% Prefer stamped-derive if the rule provides a conclusion stamp.
infer_stamp(S1, S2, T, TV, EvOut) :-
  derive(S1, S2, sentence(T, TV, EvOut)), !.

% Fallback: unstamped derive; build stamp (default: concat both premises)
infer_stamp(sentence(T1, TV1, Ev1), sentence(T2, TV2, Ev2), T, TV, EvOut) :-
  derive(sentence(T1, TV1), sentence(T2, TV2), sentence(T, TV)),
  stamp_concat(Ev1, Ev2, Base),
  stamp_concat(Base, [], EvOut).

% Single-premise version (prefer stamped)
infer_stamp(S1, none, T, TV, EvOut) :-
  derive(S1, none, sentence(T, TV, EvOut)), !.
infer_stamp(sentence(T1, TV1, Ev1), none, T, TV, EvOut) :-
  derive(sentence(T1, TV1), none, sentence(T, TV)),
  stamp_concat(Ev1, [], EvOut).

% =========================
% PLN.Derive main loop (with stamps, bounded queues)
% =========================
% pln_derive(+Tasks,+Beliefs,+Steps,+MaxSteps,+TaskQSize,+BeliefQSize,-OutTasks,-OutBeliefs)

% -------------------------
% helper for best_candidate
% -------------------------
% candidates are (Conf, Term, TV, Ev)
conf_key((Conf, _Term, _TV, _Ev), Conf).

% -------------------------
% PLN.Derive main loop (fixed inner derivation block)
% -------------------------
pln_derive(Tasks, Beliefs, Steps, MaxSteps, _TQ, _BQ, Tasks, Beliefs) :-
    (Steps > MaxSteps ; Tasks = []), !.

pln_derive(Tasks, Beliefs, Steps, MaxSteps, TaskQSize, BeliefQSize, OutTasks, OutBeliefs) :-
    best_candidate(priority_rank, none, Tasks, some(Selected)),
    Selected = sentence(XTerm, XTV, XEv),

    % (X,Y) with every belief
    findall(sentence(T,TV,Stamp),
      ( member(sentence(YT0, YTV, YEv), Beliefs),
        copy_term(sentence(YT0, YTV, YEv), sentence(YT, YTV, YEv)),  % fresh vars
        stamp_disjoint(XEv, YEv),
        infer_stamp(sentence(XTerm,XTV,XEv), sentence(YT,YTV,YEv), T, TV, Stamp)
      ),
      DerivXY),

    % (Y,X) with every belief
    findall(sentence(T2,TV2,Stamp2),
      ( member(sentence(YT0, YTV, YEv), Beliefs),
        copy_term(sentence(YT0, YTV, YEv), sentence(YT, YTV, YEv)),
        stamp_disjoint(XEv, YEv),
        infer_stamp(sentence(YT,YTV,YEv), sentence(XTerm,XTV,XEv), T2, TV2, Stamp2)
      ),
      DerivYX),

    % Single-premise from X  (use the 1-premise form)
    findall(sentence(T3,TV3,Stamp3),
      ( infer_stamp(sentence(XTerm,XTV,XEv), none, T3, TV3, Stamp3)
      ),
      DerivX),

    append(DerivXY, DerivYX, D0),
    append(D0, DerivX, Derivations0),
    unique_by_variant_fast(Derivations0, Derivations), %unique_list unique_by_variant unique_by_variant_fast unique_by_variant_seen

    without_all(Tasks, Selected, TasksMinus),
    append(TasksMinus, Derivations, Tasks1u),
    unique_by_variant_fast(Tasks1u, Tasks1), %unique_list unique_by_variant unique_by_variant_fast unique_by_variant_seen
    limit_size(Tasks1, TaskQSize, TasksLim),

    append(Beliefs, Derivations, Beliefs1u),
    unique_by_variant_fast(Beliefs1u, Beliefs1), %unique_list unique_by_variant unique_by_variant_fast unique_by_variant_seen
    limit_size(Beliefs1, BeliefQSize, BeliefsLim),
    
    %format('TRACE step ~w: SELECTED=~q DERIVED=~q~n', [Steps, Selected, Derivations]),

    Steps1 is Steps + 1,
    pln_derive(TasksLim, BeliefsLim, Steps1, MaxSteps, TaskQSize, BeliefQSize, OutTasks, OutBeliefs).


% Convenience wrappers (same arities as MeTTa variants)
pln_derive(Tasks, Beliefs, MaxSteps, TaskQSize, BeliefQSize, OutTasks, OutBeliefs) :-
    pln_derive(Tasks, Beliefs, 1, MaxSteps, TaskQSize, BeliefQSize, OutTasks, OutBeliefs).

pln_derive(Tasks, Beliefs, MaxSteps, OutTasks, OutBeliefs) :-
    pln_config(task_queue_size, TQ),
    pln_config(belief_queue_size, BQ),
    pln_derive(Tasks, Beliefs, 1, MaxSteps, TQ, BQ, OutTasks, OutBeliefs).

pln_derive(Tasks, Beliefs, OutTasks, OutBeliefs) :-
    pln_config(max_steps, MS),
    pln_derive(Tasks, Beliefs, MS, OutTasks, OutBeliefs).

% =========================
% PLN.Query
% =========================

% pln_query(+Tasks,+Beliefs,+TermIn,+MaxSteps,+TaskQSize,+BeliefQSize,-TermOut,-TV,-Ev)
% TermIn may contain variables. On success, TermOut is the chosen matching term,
% and TermIn is unified with TermOut (so caller vars get bound).
pln_query(Tasks, Beliefs, TermIn, MaxSteps, TaskQSize, BeliefQSize, TermOut, TV, Ev) :-
    pln_derive(Tasks, Beliefs, MaxSteps, TaskQSize, BeliefQSize, _TOut, BOut),
    findall( (Conf, TermI, TVi, Evi),
             ( member(sentence(TermI, TVi, Evi), BOut),
               TermI = TermIn,                 % unify for matching
               TVi = stv(_F, Conf)
             ),
             Cands),
    ( Cands = [] ->
        TermOut = TermIn, TV = stv(0.0,0.0), Ev = []
    ; best_candidate(conf_key, none, Cands, some((_Conf, TermBest, TV, Ev))),
      TermOut = TermBest,
      TermIn  = TermBest                        % bind caller’s vars
    ).

% === Wrappers ===

% fill sizes from config
pln_query(Tasks, TermIn, MaxSteps, TermOut, TV, Ev) :-
    pln_config(task_queue_size, TQ),
    pln_config(belief_queue_size, BQ),
    pln_query(Tasks, Tasks, TermIn, MaxSteps, TQ, BQ, TermOut, TV, Ev).

% KB-only call (uses Tasks as Beliefs) + default max steps
pln_query(KB, TermIn, TermOut, TV, Ev) :-
    pln_config(max_steps, MS),
    pln_query(KB, TermIn, MS, TermOut, TV, Ev).
% ---------- KB: static "engraved" implication + dynamic facts ----------
kb(KB) :-
  KB = [
    % If X is a penguin, then NOT flies(X)
    sentence(
      implication(
        inheritance(X, penguin),
        neg(inheritance(X, intset(flies)))
      ),
      stv(0.99, 0.9),
      [0]
    ),

    % dynamic part
    sentence(inheritance(sam,   raven),   stv(0.99, 0.9), [1]),
    sentence(inheritance(pingu, penguin), stv(0.99, 0.9), [2]),

    sentence(inheritance(raven,  bird),   stv(0.99, 0.9), [4]),
    sentence(inheritance(penguin,bird),   stv(0.99, 0.9), [5]),
    sentence(inheritance(bird, intset(flies)), stv(0.99, 0.9), [6])
  ].

% ---------- utilities: variant-based subtraction (drop items already known) ----------
variant_member(X, [Y|_]) :- variant(X, Y), !.
variant_member(X, [_|T]) :- variant_member(X, T).

subtraction_atom([], _Orig, []).
subtraction_atom([H|T], Orig, R) :-
  ( variant_member(H, Orig) ->
      subtraction_atom(T, Orig, R)
  ; R = [H|R1],
    subtraction_atom(T, Orig, R1)
  ).

main :-
  kb(KB),

  % 1) Query: does Sam fly?
  pln_query(KB, inheritance(sam, intset(flies)), TermOut1, TV1, Ev1),
  format("Result: ~q with truth ~q and stamp ~q~n", [TermOut1, TV1, Ev1]),

  % 2) Query: does Pingu fly?
  pln_query(KB, inheritance(pingu, intset(flies)), TermOut2, TV2, Ev2),
  format("Result: ~q with truth ~q and stamp ~q~n", [TermOut2, TV2, Ev2]).

  % 3) One-step derivation from just the Pingu->Penguin sentence
  %    Beliefs include the full KB so schematic knowledge participates.
  %OnlyTask = [
  %  sentence(inheritance(pingu, penguin), stv(0.99, 0.9), [2])
  %],
  %pln_derive(OnlyTask, KB, 1, TOut, BOut),

  % Show *new* beliefs (derived) by subtracting original KB
  %subtraction_atom(BOut, KB, NewOnly),
  %format("New derived beliefs (1 step): ~q~n", [NewOnly]).
