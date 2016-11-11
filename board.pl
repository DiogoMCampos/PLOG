board([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o]]).

boardStart([[piece(r, 3), o, o, o, o, o, o, o, piece(r, 3)],
    [o, o, piece(r, 2), piece(r, 1), piece(r, 1), piece(r, 1), piece(r, 2), o, o],
    [o, o, o, o, piece(r, 1), o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, piece(w, 1), o, o, o, o],
    [o, o, piece(w, 2), piece(w, 1), piece(w, 1), piece(w, 1), piece(w, 2), o, o],
    [piece(w, 3), o, o, o, o, o, o, o, piece(w, 3)]]).

boardMidGame([[o, o, o, o, o, o, o, o, o],
    [o, piece(r, 1), o, o, o, o, o, o, o],
    [o, o, o, o, piece(r, 1), o, o, o, o],
    [o, o, o, piece(r, 3), o, o, o, o, o],
    [o, o, o, o, o, o, piece(w, 2), o, o],
    [o, o, o, piece(w, 3), o, piece(r, 1), o, o, o],
    [o, o, piece(r, 3), o, o, o, o, o, o],
    [o, o, o, o, o, piece(w, 3), piece(w, 1), piece(r, 2), piece(r, 2)],
    [o, o, piece(w, 1), piece(w, 1), o, o, o, o, piece(w, 2)]]).

boardEndGame([[o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, piece(r, 1), o, o, o, o],
    [o, o, o, o, o, o, o, o, o],
    [o, o, o, o, o, o, piece(w, 2), o, o],
    [o, o, o, o, o, piece(r, 1), o, o, o],
    [o, o, piece(r, 3), o, o, o, o, o, o],
    [o, o, o, o, o, piece(w, 3), o, piece(r, 2), piece(r, 2)],
    [o, o, piece(w, 1), piece(w, 1), o, o, o, o, o]]).

letters(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']).

translate(o) :- write(' ').
translate(piece(w, 3)) :- write('O').
translate(piece(w, 2)) :- write('o').
translate(piece(w, 1)) :- write('.').
translate(piece(r, 3)) :- write('X').
translate(piece(r, 2)) :- write('x').
translate(piece(r, 1)) :- write('*').

pieceHeight(piece(_, Height), Height).
pieceColor(piece(Color, _), Color).

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

displayCol(_, 0).
displayCol([X|Xs], I) :-
    Next is I-1,
    write('  '),
    write(X),
    write(' '),
    displayCol(Xs, Next).

displayLine(_, _, 0) :-
    write(' | '),
    nl.
displayLine([X|Xs], N, R) :-
    R1 is R-1,
    write(' | '),
    translate(X),
    displayLine(Xs, N, R1).

displaySeparator(_, 0) :- write('+').
displaySeparator(N, R) :-
    R1 is R-1,
    write('+---'),
    displaySeparator(N, R1).

displayBoard(_, N, 0) :-
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

withinBoard(X, Y, Size) :-
    X @>= 'A',
    X @=< 'I',
    Y > 0,
    Y =< Size.

isOrthogonal(InC, InL, DeC, DeL) :-
    (InC == DeC, InL \== DeL);
    (InC \== DeC, InL == DeL).

returnResult(Result, Result).

getListElement(Index, [X|Xs], Iterator, Result) :-
    Index =\= Iterator ->
        NewIterator is Iterator+1,
        getListElement(Index, Xs, NewIterator, Result);
    returnResult(X, Result).

/* Missing Player and [X|Xs] and board size is currently hardcoded */
verifyMove(InC, InL, DeC, DeL) :-
    withinBoard(InC, InL, 9),
    withinBoard(DeC, DeL, 9).

%move([X|Xs], InC, InL, DeC, DeL).
%finish(X).

analyseMove([X|Xs], Player) :-
    askMove(InC, InL, DeC, DeL),
    verifyMove(InC, InL, DeC, DeL).

%analyseMove([X|Xs], Player) :-
%    askMove([X|Xs], InC, InL, DeC, DeL),
%    verifyMove([X|Xs], InC, InL, DeC, DeL, P).

play(X, Player, OtherPlayer) :-
    analyseMove(X, Player),
    move(InC, InL, DeC, DeL),
    finish(X),
    play(X, OtherPlayer, Player).

game(X) :-
    setupGame(X, 9),
    play(X, a, b).
