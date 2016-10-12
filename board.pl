board([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o]]).

translate(o) :- write(' ').

displayLine([]) :- nl..

displayLine([X|Xs]) :- translate(X), write('|'), displayLine(Xs).

displayBoard([]) :- nl.

displayBoard([L|Ls]) :- displayLine(L), nl, displayBoard(Ls).
