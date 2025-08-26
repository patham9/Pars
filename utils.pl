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
