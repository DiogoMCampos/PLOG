board([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o]]).

boardStart([[r3, o, o, o, o, o, o, o, r3],
    [o, o, r2, r1, r1, r1, r2, o, o],
    [o, o, o, o, r1, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, w1, o, o, o, o],
    [o, o, w2, w1, w1, w1, w2, o, o],
    [w3, o, o, o, o, o, o, o, w3]]).

boardMidGame([[o, o, o, o, o, o, o, o, o],
    [o, r1, o, o, o, o, o, o, o],
    [o, o, o, o, r1, o, o, o, o],
    [o, o, o, r3, o, o, o, o, o],
    [o, o, o, o, o, o, w2, o, o],
    [o, o, o, w3, o, r1, o, o, o],
    [o, o, r3, o, o, o, o, o, o],
    [o, o, o, o, o, w3, w1, r2, r2],
    [o, o, w1, w1, o, o, o, o, w2]]).

translate(o) :- write('  ').
translate(w3) :- write('W3').
translate(w2) :- write('W2').
translate(w1) :- write('W1').
translate(r3) :- write('R3').
translate(r2) :- write('R2').
translate(r1) :- write('R1').

game(X) :- board(X), displayBoard(X).
start(X) :- boardStart(X), displayBoard(X).
midGame(X) :- boardMidGame(X), displayBoard(X).

displayLine([]) :- write('|'), nl.
displayLine([X|Xs]) :- write('|'),translate(X),  displayLine(Xs).

displayBoard([]) :- nl.
displayBoard([L|Ls]) :- displayLine(L), nl, displayBoard(Ls).
