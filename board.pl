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

game(X) :- board(X), displayBoard(X).
displayLine([]) :- write('|'), nl.
displayLine([X|Xs]) :- write('|'),translate(X),  displayLine(Xs).

displayBoard([]) :- nl.
displayBoard([L|Ls]) :- displayLine(L), nl, displayBoard(Ls).
