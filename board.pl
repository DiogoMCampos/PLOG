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

boardEndGame([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, r1, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, w2, o, o],
    [o, o, o, o, o, r1, o, o, o],
    [o, o, r3, o, o, o, o, o, o],
    [o, o, o, o, o, w3, o, r2, r2],
    [o, o, w1, w1, o, o, o, o, o]]).

letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']).

translate(o) :- write(' ').
translate(w3) :- write('O').
translate(w2) :- write('o').
translate(w1) :- write('.').
translate(r3) :- write('X').
translate(r2) :- write('x').
translate(r1) :- write('*').

%getNth([L|Ls], 0, R):- R is L.
%   getNth([L|Ls], N, R) :- getNth(Ls, N-1, R).

%game(X, N) :- board(X), displayBoard(X, N).
start(X, N) :- boardStart(X), letters(A), displayBoard(X, N, N, A).
midGame(X, N) :- boardMidGame(X), displayBoard(X, N).
end(X, N) :- boardEndGame(X), displayBoard(X, N).

displayNum(N, N) :- write('  '), write(N).
displayNum(N, I) :- N > 0, Next is I+1, write('  '), write(I), write(' '),displayNum(N, Next).

displayLine(X, N, 0) :- write(' | '), nl.
displayLine([X|Xs], N, R) :- R >= 0, R1 is R-1, write(' | '),translate(X),  displayLine(Xs, N, R1).

displaySeparator(N, 0) :- write('+').
displaySeparator(N, R) :- R1 is R-1, write('+---'), displaySeparator(N, R1).

displayBoard(X, N, 0, A) :- write('   '), displaySeparator(N, N), nl, write('   '), displayNum(N, 1).
displayBoard([L|Ls], N, R, [A|As]) :- R1 is R-1,
                            write('   '), displaySeparator(N, N),
                            nl, write(' '), write(A),
                            displayLine(L, N, N),
                            displayBoard(Ls, N, R1, As).
