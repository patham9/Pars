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
