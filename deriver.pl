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
