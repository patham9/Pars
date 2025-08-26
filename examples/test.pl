main :-
    Tasks = [
        sentence(inheritance(a,b), stv(0.8,0.7), [e1]),
        sentence(inheritance(b,c), stv(0.9,0.6), [e2]),
        sentence(inheritance(c,d), stv(0.9,0.6), [e3])
    ],
    pln_query(Tasks, inheritance(a,d), TV, Ev),
    format("Result: ~q with stamp ~q~n", [TV, Ev]).
