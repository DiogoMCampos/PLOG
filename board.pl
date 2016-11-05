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

setupGame(X, N) :-
    N < 10,
    N >= 0,
    boardStart(X),
    displayBoard(X, N, N).

midGame(X, N) :-
    boardMidGame(X),
    displayBoard(X, N, N).

end(X, N) :-
    boardEndGame(X),
    displayBoard(X, N, N).

displayCol(X, 0).
displayCol([X|Xs], I) :-
    Next is I-1,
    write('  '),
    write(X),
    write(' '),
    displayCol(Xs, Next).

displayLine(X, N, 0) :-
    write(' | '),
    nl.
displayLine([X|Xs], N, R) :-
    R1 is R-1,
    write(' | '),
    translate(X),
    displayLine(Xs, N, R1).

displaySeparator(N, 0) :- write('+').
displaySeparator(N, R) :-
    R1 is R-1,
    write('+---'),
    displaySeparator(N, R1).

displayBoard(X, N, 0) :-
    write('   '),
    displaySeparator(N, N),
    nl,
    write('   '),
    letters(A),
    displayCol(A, N).
displayBoard([L|Ls], N, R) :-
    R1 is R-1,
    write('   '),
    displaySeparator(N, N),
    nl,
    write(' '),
    write(R),
    displayLine(L, N, N),
    displayBoard(Ls, N, R1).

askMove(InC, InL, DeC, DeL) :-
    write('Coordinates of the piece to move (ex: \'d3\'.)'),
    nl,
    read(Input),
    atom_chars(Input, [InC|[InL|Rest]]),
    write('Coordinates of the pieces destination (ex: \'f3\'.)'),
    nl,
    read(Dest),
    atom_chars(Dest, [DeC|[DeL|Rest]]).


verifyMove([X|Xs], Player, InC, InL, DeC, DeL).
move([X|Xs], InC, InL, DeC, DeL).
finish(X).

analyseMove([X|Xs], Player) :-
    askMove([X|Xs], InC, InL, DeC, DeL),
    verifyMove([X|Xs], InC, InL, DeC, DeL, P),

play(X, Player, OtherPlayer) :-
    analyseMove(Player),
    move(InC, InL, DeC, DeL),
    finish(X),
    play(X, OtherPlayer, Player).

game(X) :-
    setupGame(X, 9),
    play(X, a, b).
