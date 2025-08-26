% kb(-KB)
% Unified KB with schematic implication sentences (variables in terms are allowed)
kb(KB) :-
  KB = [
    % --- schematic knowledge ("engraved implications") ---
    % If (X,Y) are friends, then: (X smokes) -> (Y smokes)
    sentence(
      implication(
        inheritance(product(X,Y), friend),
        implication(
          inheritance(X, intset(smokes)),
          inheritance(Y, intset(smokes))
        )
      ),
      stv(0.4, 0.9),
      [1]
    ),

    % If someone smokes, they are cancerous
    sentence(
      implication(
        inheritance(X, intset(smokes)),
        inheritance(X, intset(cancerous))
      ),
      stv(0.6, 0.9),
      [2]
    ),

    % --- concrete observations ---
    sentence(inheritance(product(anna,bob),     friend), stv(1.0, 0.9), [3]),
    sentence(inheritance(product(anna,edward),  friend), stv(1.0, 0.9), [4]),
    sentence(inheritance(product(anna,frank),   friend), stv(1.0, 0.9), [5]),
    sentence(inheritance(product(edward,frank), friend), stv(1.0, 0.9), [6]),
    sentence(inheritance(product(gary,helen),   friend), stv(1.0, 0.9), [7]),
    sentence(inheritance(product(gary,frank),   friend), stv(0.0, 0.9), [8]),
    sentence(inheritance(anna,   intset(smokes)), stv(1.0, 0.9), [9]),
    sentence(inheritance(edward, intset(smokes)), stv(1.0, 0.9), [10])
  ].

% convenience runner for your example query (no echo of the query term)
main :-
  kb(KB),
  % ask for a grounded target, letting reasoning unify schematic sentences
  pln_query(KB, inheritance(edward, intset(cancerous)), TermOut, TV, Ev),
  format("Result: ~q with truth ~q and stamp ~q~n", [TermOut, TV, Ev]).
